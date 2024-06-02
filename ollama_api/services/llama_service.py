import os

from llama_index.core import SimpleDirectoryReader, VectorStoreIndex, Settings
from llama_index.core.llms import ChatMessage
from llama_index.llms.ollama import Ollama
from llama_index.embeddings.ollama import OllamaEmbedding

Settings.llm = Ollama(model="llama3", request_timeout=120.0)
Settings.embed_model = OllamaEmbedding(model_name="llama3")

class LlamaService:
    def __init__(self):
        self.llm = Ollama(model="llama3:latest", request_timeout=120.0)
        self.index = None

    def ingest_directory(self, directory_path: str):
        documents = []
        for root, dirs, files in os.walk(directory_path):
            print("ingesting directory: ")

            files = [f for f in files if f != ".DS_Store" and not f.endswith("~")]
            print(root, dirs, files)
            if any(os.path.isfile(os.path.join(root, f)) for f in files):
                reader = SimpleDirectoryReader(root)
                documents.extend(reader.load_data())
                break
        print("indexing")
        self.index = VectorStoreIndex.from_documents(documents, )
        self.index.set_index_id(directory_path)
        self.index.storage_context.persist("vector_stores")
        return {"status": "success"}

    def query_data(self, query: str):
        if self.index is None:
            return "No data indexed"
        query_engine = self.index.as_query_engine()
        response = query_engine.query(query)
        return response

    def chat(self, messages):
        formatted_messages = [
            ChatMessage(role=msg["role"], content=msg["content"])
            for msg in messages
        ]
        response = self.llm.chat(formatted_messages)
        return response

llama_service = LlamaService()
