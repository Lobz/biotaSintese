# IntegraFlora

## Como usar as listas de espécie

As listas de espécies já geradas podem ser baixadas aqui: <incluir zip>
Você pode ler ou baioxar listas individuais aqui:

## Estrutura de diretorios e conteúdo do repositório

- [analyses/](analyses/) - scripts para tratamento dos dados
    - []
- [data](data) - dados usados pela ferramenta, informações sobre bases de dados e localidades
- [data-input](data-input) - dados brutos baixados dos Herbários Virtuais
    - [GBIF](data-input/GBIF) - arquivos baixados do [GBIF](https://www.gbif.org/occurrence/search?taxon_key=6&occurrence_status=present)
    - [JABOT](data-input/JABOT) - arquivos baixados do [JABOT](https://jabot.jbrj.gov.br/v3/consulta.php)
    - [Reflora](data-input/Reflora) - arquivos baixados do [Reflora](https://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=20)
    - [splink](data-input/splink) - arquivos baixados do [splink](https://specieslink.net/search/)
- [data-tmp](data-tmp) - arquivos intermediários criados por esta ferramenta
- [plots](plots) - figuras
- [R](R) - funções usadas pelos scripts
- [results](results) - resultados, incluindo as listas de espécies
    - [allfields](results/allfields) - listas de espécies contendo todos os campos, em formato .csv
    - [checklists](results/checklists) - listas de espécies no formato do Catálogo de Plantas das UCs do Brasil
    - [total](results/total) - todos os registros encontrados em cada UC, em formato .rda
    - [total-treated](results/total-treated) - todos os registros encontrados em cada UC, em formato .csv


## Como usar esta ferramenta:

1. Antes de começar, é preciso baixar os dados atualizados das bases de dados:
- [GBIF](https://www.gbif.org/occurrence/search?taxon_key=6&occurrence_status=present) - arquivos .zip
- [Reflora](https://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=20) - arquivos .csv
- [splink](https://specieslink.net/search/) (obs.: para baixar dados em grandes quantidades, será necessário criar uma conta) - arquivos .txt
- [JABOT](https://jabot.jbrj.gov.br/v3/consulta.php) - arquivos .csv

Os dados devem ser salvos nas respectivas pastas dentro de [data-input/](data-input).
No caso de mais de um arquivo serem salvos na mesma pasta, o script combinará os dados dos arquivos diferentes antes de iniciar o tratamento dos dados.
No caso dos dados Reflora, por favor abra os arquivos e salve como csv na mesma pasta antes de prosseguir.

2.