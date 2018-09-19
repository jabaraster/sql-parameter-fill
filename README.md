# sql-parameter-fill
プレースホルダを含むSQLとそこに埋める値を組み合わせて、実行可能なSQLを組み立てます。

例えば以下の文字列を標準入力から受け取ると

```
[EL Fine]: sql: 2018-09-20 01:18:33.654--ServerSession(559065904)--Connection(621449265)--SELECT ID AS a1, AUTHORITY AS a2, CREATED AS a3, TOKEN AS a4, ACCOUNT_ID AS a5 FROM EAUTHENTICATED WHERE (AUTHORITY = ?) LIMIT ? OFFSET ?
    bind => [ADMINISTRATOR, 1, 0]
```

次のようなSQLを標準出力に書き込みます。

```sql
select
    ID as a1,
    AUTHORITY as a2,
    CREATED as a3,
    TOKEN as a4,
    ACCOUNT_ID as a5
  from
    EAUTHENTICATED
  where
    (AUTHORITY = 'ADMINISTRATOR')
  limit 1
  offset 0
```


