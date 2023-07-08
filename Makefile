HOST := $(shell hostname)

home:
	guix home reconfigure -L . -e "(@ (systemic machines $(HOST)) home)"
system:
	sudo -E guix system reconfigure -L . -e "(@ (systemic machines $(HOST)) system)"
