from fastapi import APIRouter
from pydantic import BaseModel
from services.llama_service import llama_service
from models import IngestRequest

router = APIRouter(prefix="/ingest")

@router.post("/")
async def ingest_directory(request: IngestRequest):
    result = llama_service.ingest_directory(request.directory_path)
    return {"message": result}
