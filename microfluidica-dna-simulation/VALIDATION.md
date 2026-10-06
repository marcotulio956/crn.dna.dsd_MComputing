# Validação local — 27/09/2026

Versão Studio v2. Motor MMFT do wheel original, sem alteração no código do MMFT;
DNAr fixado conforme STUDIO.md. Não é certificação de todos os circuitos possíveis.

## Resultado desta execução

- Python: 26 testes unitários passaram.
- Frontend: 4 testes passaram; TypeScript e build de produção passaram.
- Os seis JSONs da biblioteca executaram no MMFT + DNAr reais, sem avisos do detector.
- Medoids: 39 estados, 8 merges e 17 identidades de gotas, incluindo descendentes.
- Dois grupos independentes: pares corretos, sem cruzar genealogias.
- Três gotas: duas fusões binárias consecutivas, concentração final de 2 nM.
- Volumes desiguais: concentração física 5/3 nM; comparação legada 3 nM.
- A → B: erro máximo nas amostras contra solução analítica < 1e-10 nM.
- A ⇌ B: erro máximo < 7.2e-10 nM.
- 2 A → B: erro máximo < 4.4e-9 nM.
- Uma espécie sem reação: concentração constante preservada.
- Refinar a malha de 501 para 2001 pontos reduziu o erro máximo de interpolação
  do caso analítico de 0,00024015 para 0,00001613 nM. Isso não é limite universal.
- Cancelamento ativo e em fila: status cancelado mantido, resultado HTTP 409,
  nenhum contêiner órfão dos jobs de teste. Nova execução subsequente completou.

Relatórios completos são gerados em `.runs/` pelos scripts documentados em STUDIO.md.
Não versionamos dados de execução, bancos locais ou dependências instaladas.

## Interface no navegador

Verificados: simular Medoids, publicar resultado, play/pause, seek, restauração do
histórico, edição que invalida resultado e undo, visualização CRN, português/inglês,
layout em 1280×720 e 1920×1080 e arraste do divisor lateral com resize do canvas.
Em t=6000 s, a gota merge:11 mostrou S1=0,0083171 nM, CBs1=0,50832 nM e
Cs1=0,49168 nM, conferidos contra a interpolação dos dados salvos. O hover e o
inspetor usam esse mesmo relógio e distinguem valores interpolados.

## Limites científicos e de escopo

Reconstrução de merges é uma inferência única sob o modelo de eventos da versão
fixada, não genealogia nativa publicada pelo MMFT. Casos ambíguos/inconsistentes
detectados interrompem a execução; um JSON semanticamente adulterado mas coerente
pode não ser detectável. Escrita atômica e validação evitam consumir saídas parciais
ou antigas, mas não tornam a inferência infalível.

Mistura é instantânea e homogênea; não modela difusão intragota. Os modos CFD,
híbrido e continuous flow da GUI de referência não fazem parte desta versão.
Gráficos e trajetórias entre amostras são interpolados. A API antiga foi preservada
e não deve ser confundida com os endpoints v2 validados aqui.
