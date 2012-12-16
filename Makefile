CHECK=✔
HR=\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

build:
	@echo "${HR}"
	@echo "Building Up..."
	@echo "${HR}"
	@cat css/{bootstrap,custom,bootstrap-responsive,font-awesome}.css > css/up.css
	@recess --compress css/up.css > css/up.min.css
	@echo "Compressing CSS with Recess...               ${CHECK} Done"
	@echo "${HR}"
	@echo "Up successfully built."
	@echo "${HR}"
	@echo "<3 @caarlos0"

