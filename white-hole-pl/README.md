# White Hole Prolog

## O que é preciso para rodar o White Hole na sua máquina

### Distribuições Linux (Ubuntu e Fedora)

Para rodar o White Hole no linux, é preciso instalar algumas dependências. Essas dependências estão descritas logo abaixo, junto também com o comando para instalá-las, tanto no Ubuntu, quanto no Fedora.

- Para se conectar ao PostgreSQL, o Prolog utiliza o ODBC, dessa forma é preciso instalar o driver ODBC para Linux:

  - <code>sudo apt install unixodbc</code>
  
  - <code>sudo dnf install unixODBC</code>
  
- Também é necessário o driver ODBC específico para PostgreSQL:

  - <code>sudo apt install odbc-postgresql</code>
  
  - <code>sudo dnf install postgresql-odbc</code>
  
- Verifique se o arquivo <code>/etc/odbcinst.ini</code> contém o seguite bloco de código. Se não constar, adicione-o em seu arquivo <code>/etc/odbcinst.ini</code>:
Ubuntu:
```
[PostgreSQL ANSI]
Description=PostgreSQL ODBC driver (ANSI version)
Driver=psqlodbca.so
Setup=libodbcpsqlS.so
Debug=0
CommLog=1
UsageCount=1

[PostgreSQL Unicode]
Description=PostgreSQL ODBC driver (Unicode version)
Driver=psqlodbcw.so
Setup=libodbcpsqlS.so
Debug=0
CommLog=1
UsageCount=1
```
Fedora
```  
[PostgreSQL]
Description	= ODBC for PostgreSQL
Driver		= /usr/lib/psqlodbcw.so
Setup		= /usr/lib/libodbcpsqlS.so
Driver64	= /usr/lib64/psqlodbcw.so
Setup64		= /usr/lib64/libodbcpsqlS.so
FileUsage	= 1
```
- Verifique se o arquivo /etc/odbc.ini existe. Se ele existe, verifique se ele contém o seguinte bloco de código. Se não constar, crie e/ou adicione-o em seu arquivo <code>/etc/odbc.ini</code>:
  - Os valores que estão em branco devem ser substituídos pelas credenciais do banco PostgreSQL que está sendo usado. Para se conectar ao banco utilizado pela aplicação, entre em contato com um dos Mantainers do repositório.
```
[SWI-Prolog]
Description = SWI-Prolog
Driver      = {driver name} // PostgreSQL para Fedora ou PostgreSQL Unicode para Ubuntu (não tem aspas).
Servername  = 
Database    = 
UserName    = 
Password    = 
Port        = 
SSLmode     = require
```
- Teste a conexão utilizando o comando <code>iusql -v "SWI-Prolog"</code>.

- É necessário também baixar a interface do ODBC para o Prolog:

  - <code>sudo dnf install swi-prolog swi-prolog-odbc</code>

  - <code>sudo dnf install pl-odbc</code>

### Windows

- O driver ODBC para Windows já vem instalado por padrão.
- Baixe o driver do PostgreSQL ODBC para Windows. Você pode baixá-lo clicando <a href="https://www.postgresql.org/ftp/odbc/versions/msi/" target="_blank">aqui</a>.
- Para configurar o driver ODBC, pesquise no menu iniciar por ODBC e abra a opção que apareceu. Logo em seguida, adicione um novo DNS de Usuário selecionando o driver do PostgreSQL e colocando as informações necessárias para se conectar com o PostgreSQL. O nome da Data Source deve ser <code>SWI-Prolog</code>.
- Teste a conexão com o banco de dados e salve.
