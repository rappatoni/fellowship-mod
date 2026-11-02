import os, subprocess, sys
root = sys.argv[1]; os.makedirs(f"{root}/out", exist_ok=True)
lib = f"{root}/lib"; cp = ":".join(f"{lib}/{j}" for j in sorted(os.listdir(lib))) + f":{root}/classes"
only = set(sys.argv[2:])
for line in open(f"{root}/examples.txt"):
    f = line.strip(); mod = f.split("/")[0]
    cls = f.split("src/main/java/")[1][:-5].replace("/", ".")
    if only and cls.split(".")[-1] not in only: continue
    if not os.path.exists(f"{root}/classes/" + cls.replace(".", "/") + ".class"): continue
    try:
        r = subprocess.run(["java", "-cp", f"{root}/src/{mod}/src/main/resources:" + cp, cls], cwd=f"{root}/src/{mod}", capture_output=True, text=True, timeout=60)
        out, st = r.stdout + "\n--stderr--\n" + r.stderr[-3000:], f"rc={r.returncode}"
    except subprocess.TimeoutExpired as e:
        out, st = (e.stdout or b"").decode(errors="replace") if isinstance(e.stdout, bytes) else (e.stdout or ""), "TIMEOUT"
    open(f"{root}/out/{cls.split('.')[-1]}.{mod.replace('org-tweetyproject-','')}.txt", "w").write(out)
    print(st, len(out), cls, flush=True)
