CHECK=✔
HR=\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

build:
	@echo "${HR}"
	@echo "Building Up CSS file..."
	@echo "${HR}"
	@recess --compress _assets/up.less > css/up.css
	@echo "Compiling and Compressing Less and CSS files with Recess... ${CHECK} Done"
	@cat _assets/bootstrapjs/* > js/up.js.tmp
	@cat _assets/up.js >> js/up.js.tmp
	@uglifyjs -nc  js/up.js.tmp > js/up.js
	@rm -rf js/up.js.tmp
	@echo "Mixing JS files... ${CHECK} Done"
	@echo "${HR}"
	@echo "Up successfully built."
	@echo "${HR}"
	@echo "<3 @caarlos0"

