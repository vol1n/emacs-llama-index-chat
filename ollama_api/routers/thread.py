from fastapi import APIRouter
from fastapi.responses import StreamingResponse
from services.thread_service import thread_service
from services.llama_service import llama_service
from models import MessageRequest, EditMessageRequest, DeleteRequest, FirstMessageRequest

from llama_index.core.llms import ChatMessage

router = APIRouter(prefix="/thread")

import uuid

@router.post("/")
async def create_thread(request: FirstMessageRequest):
    thread_id = str(uuid.uuid1())
    message = ChatMessage(role="user", content=request.content)
    message = {"role": "user", "content": request.content}
    result = thread_service.save_thread(thread_id, [message])
    return {"thread_id": thread_id, "message": result}

@router.post("/message")
async def send_message(request: MessageRequest):
    thread_id = request.thread_id
    content = request.content
    messages = thread_service.get_thread(thread_id)
    if isinstance(messages, str):  # Thread not found
        messages = []
    messages.append({"content": content, "role": user})
    result = thread_service.save_thread(thread_id, messages)
    return {"message": result}

@router.put("/message")
async def edit_message(request: EditMessageRequest):
    result = thread_service.edit_message(request.thread_id, request.message_id, request.new_content)
    return {"message": result}

@router.delete("/message")
async def delete_message(request: DeleteRequest):
    if request.message_id is not None:
        result = thread_service.delete_message(request.thread_id, request.message_id)
    else:
        result = thread_service.delete_thread(request.thread_id)
    return {"message": result}

@router.get("/{thread_id}")
async def get_thread(thread_id: str):
    result = thread_service.get_thread(thread_id)
    return {"message": result}

@router.post("/{thread_id}/generate")
async def generate_response(thread_id: str):
    messages = thread_service.get_thread(thread_id)
    if isinstance(messages, str):  # Thread not found
        return {"message": "Thread not found"}
    
    formatted_messages = [
        ChatMessage(role=msg["role"], content=msg["content"])

        for msg in messages
    ]
    
    response = llama_service.llm.chat(formatted_messages)
    messages.append(response["message"]["raw"])
    thread_service.save_thread(thread_id, messages)
    return {"message": response}


def generate_tokens(messages, callback):
    gen = llama_service.llm.stream_chat(messages)
    full = ""
    try:
        for msg in gen:
            print("delta")
            print(msg.delta)
            full += msg.delta
            yield msg.delta
    finally:
        callback(full)

@router.post("/{thread_id}/stream")
async def stream_response(thread_id: str):
    messages = thread_service.get_thread(thread_id)
    if isinstance(messages, str):  # Thread not found
        return {"message": "Thread not found"}
    print("messages")
    print(messages)
    formatted_messages = [
        ChatMessage(role=msg["role"], content=msg["content"])

        for msg in messages
    ]
    def cb(full_message):
        #messages.append(ChatMessage(role="assistant", content=full_message))
        messages.append({"role": "assistant", "content": full_message})
        thread_service.save_thread(thread_id, messages)
    return StreamingResponse(generate_tokens(formatted_messages, cb), media_type="text/plain")
