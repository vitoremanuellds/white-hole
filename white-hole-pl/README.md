# White Hole Prolog

## O que é preciso para rodar o White Hole na sua máquina

### Distribuições Linux (Ubuntu e Fedora)

Para rodar o White Hole no linux, é preciso instalar algumas dependências. Essas dependências estão descritas logo abaixo, junto também com o comando para instalá-las, tanto no Ubuntu, quanto no Fedora.

- É preciso instalar uma pequena biblioteca para que a conexão com o PostgreSQL usado pela aplicação seja feita:

  * <code>sudo apt install libpq-dev</code>

  * <code>sudo dnf install libpq-devel</code>

- Para se conectar ao PostgreSQL, o Prolog utiliza o ODBC, dessa forma é preciso instalar o driver ODBC para Linux:

  - <code>sudo apt install unixodbc</code>
  
  - <code>sudo dnf install unixODBC</code>
  
- Também é necessário o driver ODBC específico para PostgreSQL:

  - <code>sudo apt install odbc-postgresql</code>
  
  - <code>sudo dnf install postgresql-odbc</code>
  
- Verifique se o arquivo <code>/etc/odbcinst.ini</code> contém o seguite bloco de código. Se não constar, adicione-o em seu arquivo <code>/etc/odbcinst.ini</code>:

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
[{Nome dado ao banco, fica a sua escolha}]
Description = SWI-Prolog
Driver      = /usr/lib64/psqlodbcw.so
Servername  = 
Database    = 
UserName    = 
Password    = 
Port        = 
SSLmode     = require
```
- Teste a conexão utilizando o comando <code>iusql -v "{Nome dado ao banco, fica a sua escolha}"</code>.

- É necessário também baixar a interface do ODBC para o Prolog:

  - <code>sudo dnf install pl-odbc</code>
