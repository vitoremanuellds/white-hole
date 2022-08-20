# O que é preciso para executar o White Hole

Para executar o White Hole é necessário ter instalado no seu computador o compilador Haskell (GHC), assim como o Cabal, entre outros pacotes. Para facilitar a vida de quem for testar, você pode fazer a instalação do <a href="https://www.haskell.org/ghcup/" target _blank>GHCup</a>, um gerenciador de versões do Haskell. A partir dele você pode instalar as versões do Haskell, GHC e Cabal necessárias para rodar a nossa aplicação.

Também vai ser necessário a instalação de alguns outros pacotes relacionados ao banco de dados utilizado no projeto. A depender do sistema, é preciso fazer a instalação de pacotes diferentes.

### Instalação das dependências do PostgreSQL e configuração no Windows

Para executar o projeto é preciso fazer o download do banco de dados <a href="https://www.enterprisedb.com/downloads/postgres-postgresql-downloads" target _blank>PostgreSQL</a> no sistema. Esse procedimento é necessário pois a biblioteca usada no projeto (postgresql-simple) necessita do pg_config instalado no computador para poder ser rodada.

Depois de instalado, é necessário adicionar o caminho da pasta bin (Geralmente C:/"Program Files"/PostgreSQL/<versão>/bin) no PATH do Windows. Para tal, é necessário seguir os seguintes passos:

- Pesquisar por variáveis de ambiente no menu iniciar do Windows.
Isso irá abrir uma janela onde você tem a opção de ver as variáveis de ambiente do sistema (usualmente fica localizado no canto inferior direito da janela).
- Procurar pela variável PATH.
- Editar a variável (apertando em editar )e adicionar um novo caminho, clicando em Novo.

Pronto, o projeto está pronto para funcionar.

### Instalação das dependências do PostgreSQL e configuração em distribuições Linux

Para executar o projeto, não será necessário baixar todo o PostgreSQL no sistema, somente um pacote que disponibiliza o programa pg_config.
No Ubuntu é preciso somente executar, com permissão de administrador, o seguinte comando para instalar o pacote libpq-dev:

<code>apt install libpq-dev</code>

No Fedora é preciso executar um comando, com permissão de administrador,  um pouco diferente para baixar o pacote libpq-devel (o nome realmente é um pouco diferente):

<code>dnf install libpq-devel</code>


Logo após disso, o projeto está pronto para funcionar.

## Como configurar a conexão do banco de dados

White Hole usa o PostgreSQL e se conecta a ele por auxílio da biblioteca postgresql-simple e utiliza, por motivos de segurança, variáveis de ambientes para abrigar as credenciais do banco de dados. Para utilizar o banco é preciso ter em seu sistema as seguintes variáveis de ambiente:

- dbhost
- db
- dbuser
- dbpassword

Para criar essas variáveis de ambiente no Windows, você precisa pesquisar no menu iniciar por variáveis de ambientes, abre o programa que foi achado, clica em variáveis de ambiente e clica em Novo, onde você vai colocar o nome da variável, do mesmo jeito que está escrito acima, e os valores que você pode adquirir conosco. <strong>Entre em contato conosco para adquirir as credenciais para se conectar</strong>.

No linux, basta usar o comando <code>export nome="valor"</code> trocando nome pelos nomes acimas e o valor pelas credenciais adquiridas conosco. Para fazer essas variáveis de ambiente persistirem, é preciso colocar esses comandos no final do arquivo .bashrc que fica na /home do usuário.