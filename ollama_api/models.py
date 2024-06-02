from pydantic import BaseModel

class IngestRequest(BaseModel):
    directory_path: str

class QueryRequest(BaseModel):
    query: str

class MessageRequest(BaseModel):
    thread_id: str
    content: str

class EditMessageRequest(BaseModel):
    thread_id: str
    message_id: str
    new_content: str

class DeleteRequest(BaseModel):
    thread_id: str
    message_id: str = None  # Optional for deleting entire threads
