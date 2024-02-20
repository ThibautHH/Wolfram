##
## EPITECH PROJECT, 2024
## Wolfram
## File description:
## Makefile
##

PROGRAM			:=	wolfram
SRC_DIRS		:=	src/ app/
$(PROGRAM)_SRC	:=	$(shell $(SRC_DIRS:%=find % -name "*.hs";))

BUILD_DIR		:=	$(shell stack path --local-install-root)/bin/
BUILD_ARTIFACT	:=	$(BUILD_DIR)$(PROGRAM)

all: $(PROGRAM)

$(PROGRAM): $(BUILD_ARTIFACT)
	@ln -sf $(BUILD_ARTIFACT) $(PROGRAM)

$(BUILD_ARTIFACT): $($(PROGRAM)_SRC)
	@stack build

clean:
	@stack clean

fclean: clean
	@rm -f $(PROGRAM)

re: fclean all

.PHONY: all clean fclean re
