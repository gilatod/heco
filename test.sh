export TOKEN="root:Milvus"

curl --request POST \
--url "http://localhost:19530/v2/vectordb/entities/get" \
--header "Content-Type: application/json" \
-d '{
    "dbName": "heco",
    "collectionName": "main",
    "id": [
        456429282842117008
    ],
    "outputFields": [
        "id"
    ]
}'