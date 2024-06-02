from fastapi import APIRouter
from pydantic import BaseModel
from services.llama_service import llama_service
from models import QueryRequest

router = APIRouter(prefix="/query")

@router.post("/")
async def query_data(request: QueryRequest):
    result = llama_service.query_data(request.query)
    return {"message": result}
