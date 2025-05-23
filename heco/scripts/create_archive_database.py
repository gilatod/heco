from pymilvus import (
    MilvusClient,
    DataType,
    Function,
    FunctionType,
    AnnSearchRequest,
    RRFRanker,
)

analyzer_params = {
    "type": "chinese"
}

schema = MilvusClient.create_schema()
schema.add_field(
    field_name="id",
    datatype=DataType.INT64,
    is_primary=True,
    auto_id=True
)
schema.add_field(
    field_name="content",
    datatype=DataType.VARCHAR,
    max_length=65535,
    analyzer_params=analyzer_params,
    enable_match=True,
    enable_analyzer=True,
)
schema.add_field(
    field_name="sparse_vector",
    datatype=DataType.SPARSE_FLOAT_VECTOR)
schema.add_field(
    field_name="vector",
    datatype=DataType.FLOAT_VECTOR,
    dim=1024,
)
schema.add_field(
    field_name="metadata",
    datatype=DataType.JSON
)

bm25_function = Function(
    name="bm25",
    function_type=FunctionType.BM25,
    input_field_names=["content"],
    output_field_names="sparse_vector",
)

schema.add_function(bm25_function)

index_params = MilvusClient.prepare_index_params()
index_params.add_index(
    field_name="sparse_vector",
    index_type="SPARSE_INVERTED_INDEX",
    metric_type="BM25",
)
index_params.add_index(
    field_name="vector",
    index_type="FLAT",
    metric_type="IP"
)

uri = "http://localhost:19530"
collection_name = "archives"
client = MilvusClient(uri=uri, db_name='heco')

if client.has_collection(collection_name):
    client.drop_collection(collection_name)

client.create_collection(
    collection_name=collection_name,
    schema=schema,
    index_params=index_params,
    consistency_level='Strong'
)

print(f"Collection '{collection_name}' created successfully")