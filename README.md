
## Dropbox Auth
Run the following to save a file called `ss_token.rds` in the same directory as the shiny app.

```
rdrop2::token <- drop_auth()
save(db_token, file="ss_token.rds")
```