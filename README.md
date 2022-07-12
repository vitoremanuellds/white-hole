# White Hole
## O que é o White Hole
White Hole é um programa de recomendação de filmes, onde o usuário cria uma conta e pode avaliar filmes que já assistiu. Ele também pode pesquisar por filmes e ser recomendado qual assistir.
## Como configurar a conexão do banco de dados
White Hole usa o PostgreSQL-Simple e se conecta a ele via variáveis de ambientes:

- dbhost
- db
- dbuser
- dbpassword

Para criar essas variáveis de ambiente no windows, você precisa abrir a página de Sistema nas configurações e abrir as configurações avançadas do sistema. Nela, você clica em variáveis de ambiente e adiciona uma nova com os nomes acima e com os valores que os administradores do projeto lhe passarem. Você também pode usar um banco de dados local se quiser.
No linux, basta usar o comando <code>EXPORT nome="valor"</code>.
