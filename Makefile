PACKAGE_DIR = .
NODE_MODULES = $(PACKAGE_DIR)/node_modules
BUILD_DIR = $(PACKAGE_DIR)
SRC_DIR = $(PACKAGE_DIR)
EL_FILE = ob-javascript.el
JS_FILES = util.js repl.js
BABELRC = .babelrc

NPM_INSTALL_CMD = npm install

.PHONY: all clean build

all: build

build: npm_dependencies el_compile

# Install npm dependencies
npm_dependencies:
	@echo "Installing npm dependencies..."
	@cd $(PACKAGE_DIR) && $(NPM_INSTALL_CMD)


el_compile:
	@echo "Compiling elisp files..."
	@emacs --batch -f batch-byte-compile $(EL_FILE)

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(NODE_MODULES)
	@rm -f $(EL_FILE:.el=.elc)