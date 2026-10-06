# fluiDNA Studio v2

Nova aplicação independente, ao lado da interface original em `frontend/new-interface`.
O código original, seus endpoints e seu Dockerfile não foram substituídos. **Use a API
v2 para obter o detector novo**; a API legada continua com o comportamento antigo.

## Executar

Requisitos: Windows, Docker Desktop com contêineres Linux, Python 3.11 e Node.js
22.12+ (Node 24 também foi usado nos testes). O bundle MMFT permanece Python 3.10
Linux dentro da imagem, separado do Python do servidor Windows.

```powershell
cd 'C:\Users\João\Documents\Togarashi AI Context\FLUIDNA\app'
.\start-studio.ps1 -Build  # primeira instalação / após alterações
.\start-studio.ps1         # próximas execuções
```

Abra http://127.0.0.1:8000. O launcher não modifica a política de execução do
PowerShell. Em máquinas que bloqueiam scripts, execute os comandos equivalentes:

```powershell
docker build -f Dockerfile.engine -t fluidna-engine:v2 .
cd frontend/studio
npm ci
npm run build
cd ../..
.venv/Scripts/python.exe -m uvicorn backend.api:app --host 127.0.0.1 --port 8000 --workers 1
```

Para desenvolvimento, execute `npm run dev` em `frontend/studio`; Vite usa 5173 e
encaminha `/api` ao servidor local. Não exponha essa API sem autenticação na rede.

## Uso

- Importe um JSON antigo ou escolha um dos seis exemplos na biblioteca. O arquivo
  original fica preservado em `provenance.original`, junto dos avisos de migração.
- No canvas: `+` adiciona nós; a ferramenta de canais conecta dois nós existentes;
  arraste nós no modo de seleção, use a roda para zoom e a mão para pan. Cruzamentos
  de linhas **não** criam junções. Marque explicitamente um nó como saída/ground.
- Selecione canais para editar largura/altura e adicionar gotas. Bombas são
  componentes explícitos. Cada gota tem tempo, posição, volume e composição próprios.
- Shift seleciona vários objetos; Undo/Redo, duplicação e exclusão preservam um
  histórico de até 100 edições. A exclusão de um nó remove suas conexões/injeções.
- Painéis laterais podem ser redimensionados pelos separadores ou recolhidos.
  Gráficos são opcionais para preservar a área do canvas e têm altura ajustável.
- Química: espécies, cores, visibilidade, reações em lote, coeficientes e reações
  reversíveis. `2 A + B -> C ; 0.0028` expande a estequiometria explicitamente.
  Uma reação reversível é compilada em duas reações DNAr.
- CRN: equações e grafo bipartido, com nós de espécies compartilhados e nós de
  reações. A busca reduz a rede mostrada sem alterar a simulação.
- Simular: MMFT e DNAr executam em subprocesso/contêiner isolado. Cancelamento e
  timeout não publicam um resultado parcial como sucesso.
- Play/Pause, velocidade, loop, slider e passos de estado usam o mesmo relógio.
  Eventos com timestamps iguais continuam distinguíveis pelo ordinal do estado.
- Hover mostra concentração no tempo atual; clicar fixa a inspeção de todas as
  espécies. A lista de eventos oferece acesso equivalente por teclado/touch.
- Gráficos: gota selecionada, média ponderada por volume ou quantidade total das
  gotas ativas. Quantidades usam **fmol**, concentrações **nM**, tempos **s**.
  A saída da rede remove a gota da agregação; isso não representa degradação química.
- Ajustes → Histórico de execuções: reabra resultados ou escolha uma referência
  para comparação tracejada. Para comparar diluição física com soma legada, simule
  cada modo e selecione a execução anterior como referência. Compara linhagens e
  espécies compatíveis; não relaciona gotas diferentes apenas pelo índice da tabela.
- Reciclagem copia a composição do instante inspecionado para uma injeção do projeto;
  isso invalida visualmente os resultados antigos, sem alterá-los.
- Exportações: projeto v2, resultado JSON, CSV químico, PNG do canvas e SVG da rede.
  O SVG exporta a rede estática; PNG inclui o instante visível da reprodução.

## Arquitetura

```text
frontend/studio (React + TypeScript + Vite + Konva)
  ↕ JSON v2, IDs estáveis, parâmetros SI / química nM e s
backend/api.py (FastAPI, fila única, SQLite)
  → .runs/<UUID>/project.json
  → contêiner isolado, sem rede, limite de memória/CPU/tempo
      engine.py → wheel MMFT original → mmft.json + manifest.json
      merges.py → eventos explicados + linhagens + ciclos de vida
      chemistry.py → react.R → DNAr → química por ID
      worker.py → result.json atômico
```

SQLite guarda o status; cada execução tem arquivos e logs próprios. O processo
MMFT precisa terminar com sucesso e o JSON precisa passar pela validação antes
da química. O manifesto inclui a versão e SHA-256 do JSON bruto. Reiniciar a API
marca jobs interrompidos como falha, nunca recupera arquivos antigos como sucesso.
Use **um processo de API**; esta implementação é local-first, não um cluster.

## Detector e limites científicos

`backend/merges.py` inverte as regras de `MergingEvent.hh` do commit
`f6e155442f6d71e7d4634297670df4c8516ac973`. Usa soma dos volumes, todas as
fronteiras orientadas, posição, canais totalmente ocupados e topologia. Resolve
atribuições globais, mantém pais imediatos e conserva cascatas binárias, inclusive
em intervalos positivos muito pequenos. IDs não são inferidos de nomes de nós.

Uma explicação única é rotulada `unique-under-pinned-event-model`, **não** como
genealogia diretamente fornecida pelo motor. Nenhum método externo consegue
garantir recuperar informação que o bundle não exportou. Casos ambíguos ou não
explicados bloqueiam a publicação do resultado químico. Não há fallback para a
heurística antiga. O suporte é deliberadamente restrito ao wheel incluído;
migrar para MMFT 0.3 exige outro adaptador e testes próprios.

Os pais consumidos permanecem no JSON, mas são retirados da população ativa pela
linhagem reconstruída. Saídas requerem contato com sink **e** geometria terminal
congelada; uma gota parada em outro ponto não é tratada como saída. Tempos próximos
não são arredondados para agrupar eventos. JSON válido mas semanticamente adulterado
ou snapshots omitidos por um motor diferente não são uma entrada garantida.

Mistura padrão: `c_f = Σ(c_i V_i)/ΣV_i`. O modo `legacy_sum` é apenas comparação
histórica e **não conserva quantidade de matéria**. A química começa na injeção e
termina no merge, sink ou término do motor. Não há incubação pré-injeção implícita.
O modelo representa mistura instantânea/homogênea; difusão incompleta, reação
interfacial e atraso de homogeneização não são modelados.

DNAr está fixado no commit `717b07068230935cc82503295e3741126f880a08`. Usa integração
adaptativa deSolve e malha de saída linear + logarítmica configurável, calculada em
tempo relativo para preservar intervalos curtos. A interface sinaliza interpolação;
rtol/atol do solver não são uma garantia de erro da interpolação entre amostras.
Refine o número de amostras para validar um circuito novo. Reações formais com três
reagentes foram testadas no DNAr `react`; isso não implica suporte à compilação
física DNA de todas as ordens de reação em outros modos do DNAr.

Trajetórias interpolam posições dentro dos canais. Mudanças de topologia são
eventos discretos; não se interpola uma reta atravessando o chip entre canais.
O desenho tem largura mínima em pixels para legibilidade e não é uma medida
metrológica do tamanho visual da gota.

## Migração de valores legados

| Dado legado | Interpretação efetiva preservada |
|---|---|
| Coordenadas x/y | 10 µm por unidade |
| `edge.height` | largura × 10 µm; altura física 30 µm |
| Volume armazenado | original arquivado; efetivo antigo 0,225 nL |
| Bomba automática | 1 por gota legada, inclusive duplicatas, com aviso |
| Concentração 1 ou 2 | 1 ou 2 nM |
| k de ordem n | nM^(1−n)/s, sem conversão numérica silenciosa |

Em projetos novos, todos esses parâmetros são editáveis e bombas não são criadas
implicitamente ao adicionar gotas. Nomes de espécies são identificadores DNAr
(letras, números, underscore), IDs são separados dos nomes e das posições em arrays.

## Testes

```powershell
.venv/Scripts/python.exe -m pip install pytest==8.3.5 httpx==0.28.1
.venv/Scripts/python.exe -m pytest tests -q
.venv/Scripts/python.exe tests/integration.py       # API ativa + Docker
.venv/Scripts/python.exe tests/validate_physics.py  # oráculos físicos/químicos
.venv/Scripts/python.exe tests/validate_jobs.py     # fila e cancelamento reais
cd frontend/studio
npm test
npm run build
```

Relatórios gerados: `.runs/validation.json`, `.runs/physics-validation.json` e
`.runs/jobs-validation.json`. Veja também [VALIDATION.md](VALIDATION.md).
Cobertura: seis circuitos, merge simultâneo independente, cascata de três gotas,
conservação em volumes diferentes, fronteiras permutadas, ambiguidade, JSON inválido,
tempos próximos, saídas próximas, ausência de reações, solução analítica e refinamento
de amostragem. Testes do frontend cobrem relógio, seek reversível, visibilidade,
fronteiras completas e interpolação. Testes reais validam o motor, não um mock.

## Referência da GUI e escopo

GUI oficial baixada em `../references/mmft-simulator-gui-cute`, commit
`a43ee66fe6e60b426eb1f292e4126df51af600b9`. Nenhum código ou asset dessa GUI foi
reutilizado. Sua organização de edição, propriedades e transporte serviu de
referência funcional. O novo frontend usa componentes HTML/CSS próprios e Konva;
gráficos e CRNs usam SVG, evitando dois sistemas de componentes concorrentes.

Escopo desta versão: gotículas abstratas e CRNs. Continuous/mixture flow, híbrido,
CFD, exportação de fabricação STL e toda a configuração desses modos não fazem
parte desta integração. Não há alegação de paridade com esses módulos. MMFT e
DNAr mantêm suas licenças upstream; confira os arquivos de licença dos respectivos
repositórios antes de redistribuir uma imagem pública.
