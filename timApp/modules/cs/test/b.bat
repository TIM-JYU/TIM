call curl -H "Content-Type: application/json" --data-binary @html.json localhost:5000/html
call curl -H "Content-Type: application/json" --data-binary @html.json "localhost:5000/html/?review=True"
call curl -H "Content-Type: application/json" --data-binary @mhtml.json "localhost:5000/multihtml/?review=False"
call curl -H "Content-Type: application/json" --data-binary @mhtml.json "localhost:5000/multihtml/?review=True"
