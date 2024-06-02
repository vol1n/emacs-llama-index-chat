from fastapi import FastAPI
from routers import ingest, query, thread

app = FastAPI()

app.include_router(ingest.router)
app.include_router(query.router)
app.include_router(thread.router)

@app.get("/")
async def root():
    return {"message": "Welcome to the Llama Index API"}
