# Trabalho: verificação e evolução do suporte a SBML

## Objetivo

Verificar o suporte atual a SBML no DNAr e propor/implementar os próximos
passos para ampliar a compatibilidade com os níveis e versões do formato.

## Parte 1 — Reproduzir o suporte atual

1. Abra o projeto no container de desenvolvimento:

   ```bash
   docker build -t dnar-dev -f .devcontainer/Dockerfile .
   docker run --rm -it -v "$PWD":/workspaces/dnar \
     -w /workspaces/dnar --name dnar-dev dnar-dev bash
   ```

2. No container, confirme que libSBML está disponível:

   ```bash
   python3 -c "import libsbml; print(libsbml.getLibSBMLDottedVersion())"
   ```

3. Execute o teste do caso fornecido:

   ```bash
   R --vanilla
   ```

   ```r
   source("R/sbml.R")
   validate_sbml("examples/sbml/cases/00028.xml")
   model <- read_sbml_to_dnar("examples/sbml/cases/00028.xml")
   print(model)
   ```

   Verifique se o documento é válido e se o modelo contém `S1`, `S2`,
   `S1 -> S2` e os valores iniciais esperados.

4. Execute uma simulação DNAr usando `model$species`, `model$ci`,
   `model$reactions` e `model$ki`. Registre a saída em CSV, mantendo a coluna
   `time` e uma coluna para cada espécie.

5. Baixe ou disponibilize localmente os casos da [SBML Test
   Suite](https://sbml.org/software/sbml-test-suite/) e, para cada caso
   escolhido:

   - leia o arquivo SBML e o arquivo de configurações;
   - gere os tempos indicados (`start`, `duration` e `steps`);
   - importe o modelo com `read_sbml_to_dnar()`;
   - simule com o mesmo método numérico para todos os casos;
   - compare cada espécie com o arquivo de referência.

   A comparação deve usar erro absoluto e relativo, por exemplo:

   ```r
   max(abs(resultado - referencia))
   max(abs(resultado - referencia) /
       pmax(abs(referencia), 1e-12))
   ```

   Defina antes os limites de aprovação (sugestão: `1e-6` absoluto ou
   `1e-4` relativo), documentando casos que falharem por diferenças de solver,
   unidades ou funcionalidades ainda não implementadas.

## _Deliverables_

Entregar:

- um script reprodutível para executar os casos;
- uma tabela com caso, nível/versão SBML, resultado da validação, erro máximo e
  situação (`passou`, `falhou` ou `não suportado`);
- uma análise breve de pelo menos um caso aprovado e um caso não suportado;
- testes automatizados para qualquer correção ou extensão realizada.

## Situação atual e próximos níveis de suporte

O suporte atual usa libSBML para validar e ler documentos, importa espécies,
compartimentos, concentrações/quantidades iniciais, parâmetros e
estequiometria básica, e converte parte das leis cinéticas para o formato
`species/ci/reactions/ki` do DNAr. Leis não mass-action são preservadas como
metadados e atualmente recebem uma aproximação pela taxa no estado inicial.
Isso não constitui suporte completo à semântica SBML.

Evoluir o suporte nesta ordem:

1. **Cobertura básica:** SBML Level 2/3 Core, unidades, compartimentos,
   espécies, parâmetros e reações mass-action; validar e passar os casos
   correspondentes da Test Suite.
2. **Leis cinéticas gerais:** avaliar MathML/libSBML com dependências explícitas
   de tempo, espécies e parâmetros, em vez de reduzir a lei a um `ki`
   constante.
3. **Eventos e regras:** implementar assignment, rate e algebraic rules,
   eventos, prioridades e condições iniciais.
4. **Recursos de modelo:** tratar species/compartment/parameter `constant`,
   `boundaryCondition`, conversões de unidade e condições iniciais ausentes
   conforme a especificação.
5. **Pacotes Level 3:** adicionar suporte selecionado a pacotes como
   Layout/Render, FBC ou comp, sempre delimitando o subconjunto implementado.
6. **Conformidade:** executar a suíte completa em CI, publicar a matriz de
   compatibilidade Level/Version/Package e manter tolerâncias e casos de
   regressão versionados.

Cada etapa deve incluir documentação, testes positivos e negativos, comparação
com as referências da SBML Test Suite e uma decisão explícita sobre o que ainda
não é suportado.
