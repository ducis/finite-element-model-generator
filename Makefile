ANSYS="C:\Program Files\Ansys Inc\v100\ANSYS\bin\intel\ansys100.exe"
proj:
	$(ANSYS) -b -i proj.dat -o proj.out
txt3:
	$(ANSYS) -b -i 3.txt -o 3.txt.out
test:
	$(ANSYS) -b -i HW_template_ref.dat -o 123.out
ag:
	$(ANSYS) -b -i ansys-input.dat
view_0:
	$(ANSYS) -i 3.txt
view_1:
	$(ANSYS) -i 1.txt
view_2:
	$(ANSYS) -i ansys-input.dat
clean:
	rm *.out *.log *.err file.bat
gen:
	runghc gen.hs>ansys-input.dat
full:
	cat mb.dat 1002.txt arm.txt shoulder.dat cp.dat ld.dat > full.dat

