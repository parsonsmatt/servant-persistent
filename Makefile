ghcid-devel:
	ghcid \
	    --command "stack ghci servant-persistent" \
	    --test "DevelMain.update"

.PHONY: ghcid-devel

