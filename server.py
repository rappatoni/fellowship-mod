from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List

from wrap.cli import setup_prover
from core.dc.argument import Argument
from core.comp.color import DebateTermLabeller

# --- Models ---
class ArgumentRequest(BaseModel):
    declarations: List[str] = [] # Added this!
    name: str
    conclusion: str
    instructions: List[str]
    is_anti: bool = False

class EvaluateResponse(BaseModel):
    name: str
    status: str

# --- App Initialization ---
app = FastAPI(title="AIDA Backend")

# Enable CORS so local Monaco web editor can talk to API
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"], 
    allow_methods=["*"],
    allow_headers=["*"],
)

STATUS_MAP = {
    "green": "ACCEPTED",
    "red": "DEFEATED",
    "yellow": "OPEN",
}

# --- Endpoints ---
@app.post("/evaluate", response_model=EvaluateResponse)
def evaluate_argument(req: ArgumentRequest):
    prover = setup_prover()
    try:
        prover.send_command("lk.")
        
        # Apply any declarations before creating the argument
        for decl in req.declarations:
            prover.send_command(decl)
            
        # 1. Construct and execute the argument
        arg = Argument(
            prover,
            name=req.name,
            conclusion=req.conclusion,
            instructions=req.instructions, # Ensure no trailing dots in these!
            is_anti=req.is_anti
        )
        arg.execute()
        
        # 2. Normalize and extract the color label
        if arg.normal_body is None:
            arg.normalize()
            
        raw_status = DebateTermLabeller().status_of(arg.normal_body)
        final_status = STATUS_MAP.get(raw_status, raw_status.upper())
        
        return EvaluateResponse(name=arg.name, status=final_status)
        
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))
    finally:
        # Closing the binary to prevent zombie processes
        prover.close()