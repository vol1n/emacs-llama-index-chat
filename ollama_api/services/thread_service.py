import os
import json

class ThreadService:
    def __init__(self, storage_path="threads"):
        self.storage_path = storage_path
        os.makedirs(self.storage_path, exist_ok=True)

    def save_thread(self, thread_id, messages):
        with open(os.path.join(self.storage_path, f"{thread_id}.json"), "w") as f:
            json.dump(messages, f)
        return "Thread saved"

    def delete_thread(self, thread_id):
        try:
            os.remove(os.path.join(self.storage_path, f"{thread_id}.json"))
            return "Thread deleted"
        except FileNotFoundError:
            return "Thread not found"

    def edit_message(self, thread_id, message_id, new_content):
        message_id = int(message_id)
        thread_path = os.path.join(self.storage_path, f"{thread_id}.json")
        if not os.path.exists(thread_path):
            return "Thread not found"
        
        with open(thread_path, "r") as f:
            messages = json.load(f)

        if message_id < 0 or message_id >= len(messages):
            return "Message ID out of range"

        messages[message_id]['content'] = new_content

        with open(thread_path, "w") as f:
            json.dump(messages, f)
        return "Message edited"

    def delete_message(self, thread_id, message_id):
        message_id = int(message_id)
        thread_path = os.path.join(self.storage_path, f"{thread_id}.json")
        if not os.path.exists(thread_path):
            return "Thread not found"
        
        with open(thread_path, "r") as f:
            messages = json.load(f)

        if message_id < 0 or message_id >= len(messages):
            return "Message ID out of range"

        del messages[message_id]

        with open(thread_path, "w") as f:
            json.dump(messages, f)
        return "Message deleted"
    
    def get_thread(self, thread_id):
        try:
            with open(os.path.join(self.storage_path, f"{thread_id}.json"), "r") as f:
                messages = json.load(f)
            return messages
        except FileNotFoundError:
            return "Thread not found"

thread_service = ThreadService()
