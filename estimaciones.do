clear all
set more off
cd "C:\Users\david baena\Desktop\Mismatch\nuevos_datos"

import delimited using "stata.csv"

*Últimos detalles
foreach var of varlist _all {
	capture confirm string variable `var'
	if !_rc {
		replace `var' = "." if `var' == "NA"
	}
}

gen otros_medios =  camiónpequeño1535ton + moto2tiempos + moto4tiempos + motocarro + taxi + bicicletasinmotor + bicicletaconmotor + otro
gen carro = automóvilcompacto1300cc + automóvilmediano13001800cc + camionetapickup15ton

xi i.carro, noomit
rename _Icarro_0 carro0
rename _Icarro_1 carro1
gen carro2 = _Icarro_2 + _Icarro_3 + _Icarro_4 + _Icarro_5
rename _Icarro_2 carro2a
gen carro3 = _Icarro_3 + _Icarro_4 + _Icarro_5
rename _Icarro_3 carro3a
rename _Icarro_4 carro4
rename _Icarro_5 carro5

gen carros = 0 if carro0 == 1
replace carros = 1 if carro1 == 1
replace carros = 2 if carro2a == 1
replace carros = 3 if carro3 == 1


gen carrosa = 0 if carro0 == 1
replace carrosa = 1 if carro0 == 0

destring mismatch, replace

generate empleado = 1 if desempleado == 0
replace empleado = 0 if desempleado == 1

drop if (mismatch == . & desempleado == 0) //trabajador empleado en area desconocida sin educación que no tiene trabajo parcial, completo o voluntorio
replace mismatch = . if desempleado == 1 //desempleados que trabajan 

forvalues i = 1/7 {
	destring modelo_veh_`i', replace force
	replace modelo_veh_`i' = 0 if modelo_veh_`i' == .
}

*Número de familias
egen familias = group(id_hogar)
quietly sum familias
dis "El número de familias es " r(max)
drop familia

destring overeduc, replace
destring undereduc, replace
destring mideduc, replace

egen instrumento = rowmax(modelo_veh_1 modelo_veh_2 modelo_veh_3 modelo_veh_4 modelo_veh_5 modelo_veh_6 modelo_veh_7)
egen instrumento2 = max(instrumento), by(sit_o)

gen acc = accesiblidad_privada_nivel*carrosa
quietly sum acc
replace acc = (acc - r(mean))/r(sd)

replace accesiblidad = cond(otros_medios == 0, accesiblidad_nivel, accesiblidad_nivel/otros_medios)
quietly sum accesiblidad
replace accesiblidad = (accesiblidad - r(mean))/r(sd)

destring desc_escolaridad_max, replace 
destring edad_promedio, replace 
gen correccion_stata = desc_escolaridad_max - edad_promedio
replace mismatch = correccion_stata
quietly sum mismatch
replace mismatch = (mismatch - r(mean))/r(sd)

destring mismatch_alt, replace
quietly sum mismatch_alt
replace mismatch_alt = (mismatch_alt - r(mean))/r(sd)

corr dist_parqueaderos parqueaderos dist_parquea_bici parquea_bici dist_taxi taxi dist_bus bus dist_metro metro parqueaderos_den parquea_bici_den taxi_den bus_den

*Cambiamos de metros a kilómetros las variables instrumentales para aumentar desv. est.
local cambiar dist_parqueaderos dist_parquea_bici dist_taxi dist_bus dist_metro
foreach var of local cambiar {
	replace `var' = `var'/1000
}

xi i.parentesco, noomit
rename _Iparentesc_1 head_household
*keep if medellin == 1

*Estimaciones
global amenidades artes banco bar biblioteca bomberos cafe cajapostal cajero casino cine cementerio clinica clubnocturno colegio college comidarapida coworking doctor embajada farmacia gasolina hospital iglesia lavado_carro mercado odontologo oficinapostal policia prision pub restaurantes teatro universidad veterinaria planetario
*Si no aceptan componentes principales sumamos
*gen amenity = artes + banco + bar + biblioteca + bomberos + cafe + cajapostal + casino + cajero + cine + cementerio + clinica + clubnocturno + college + colegio + comidarapida + coworking + doctor + embajada + farmacia + gasolina + hospital + iglesia + mercado + lavado_carro + odontologo + oficinapostal + policia + prision + pub + teatro + restaurantes + universidad + veterinaria + planetario
pca $amenidades, comp(20) mine(1)
*search fapara
*El análisis paralelo indica que la cantidad óptima de componentes es 14
fapara, pca reps(1000)
*La rule of thumb del eigenvalue superior a uno indica lo mismo: 14 componentes
*screeplot, yline(1) ci(het)
predict pc1 pc2 pc3 pc4 pc5 pc6 pc7 pc8 pc9 pc10 pc11 pc12 pc13, score
global amenity pc1 pc2 pc3 pc4 pc5 pc6 pc7 pc8 pc9 pc10 pc11 pc12 pc13

gen celda1 = 0
replace celda1 = 1 if celda_propia == "TRUE"

gen vivienda1 = 0
replace vivienda1 = 1 if desc_vivienda_es == "Alquilada"

set more off
cmp (acc = celda1 vivienda1 parquea_bici_den parquea_bici dist_parqueadero metro_den dist_parquea_bici dist_taxi dist_bus dist_metro instrumento2 ///
	 edad04 edad59 edad1015 edad1619 casado genero ///
	 head_household edad c.edad#c.edad bachillerato educaciónnoformal ninguno /// 
	 noresponde novenogrado posgrado primaria técnico tecnológico universitario $amenity) ///
	(accesiblidad = celda1 vivienda1 parquea_bici_den parquea_bici dist_parqueadero metro_den  dist_parquea_bici dist_taxi dist_bus dist_metro instrumento2 ///
	 edad04 edad59 edad1015 edad1619 casado genero ///
	 head_household edad c.edad#c.edad bachillerato educaciónnoformal ninguno ///
	 noresponde novenogrado posgrado primaria técnico tecnológico universitario $amenity) ///
	(undereduc = accesiblidad acc edad04 edad59 edad1015 edad1619 casado genero ///
	 head_household edad c.edad#c.edad bachillerato educaciónnoformal ninguno ///
	 noresponde novenogrado posgrado primaria técnico tecnológico universitario $amenity) ///
	(overeduc = accesiblidad acc edad04 edad59 edad1015 edad1619 casado genero ///
	 head_household edad c.edad#c.edad bachillerato educaciónnoformal ninguno ///
	 noresponde novenogrado posgrado primaria técnico tecnológico universitario $amenity), ///
	 ind($cmp_cont $cmp_cont $cmp_probit $cmp_probit) 

	 
outreg2 using "regresion_cmp.xls"

gen educ = 1
replace educ = 2 if mideduc == 1
replace educ = 3 if overeduc == 1
set more off
cmp (acc = celda1 vivienda1 parquea_bici_den parquea_bici dist_parqueadero metro_den dist_parquea_bici dist_taxi dist_bus dist_metro instrumento2 ///
	 edad04 edad59 edad1015 edad1619 casado genero ///
	 head_household edad c.edad#c.edad bachillerato educaciónnoformal ninguno /// 
	 noresponde novenogrado posgrado primaria técnico tecnológico universitario $amenity) ///
	(accesiblidad = celda1 vivienda1 parquea_bici_den parquea_bici dist_parqueadero metro_den  dist_parquea_bici dist_taxi dist_bus dist_metro instrumento2 ///
	 edad04 edad59 edad1015 edad1619 casado genero ///
	 head_household edad c.edad#c.edad bachillerato educaciónnoformal ninguno ///
	 noresponde novenogrado posgrado primaria técnico tecnológico universitario $amenity) ///
	(educ = accesiblidad acc edad04 edad59 edad1015 edad1619 casado genero ///
	 head_household edad c.edad#c.edad bachillerato educaciónnoformal ninguno ///
	 noresponde novenogrado posgrado primaria técnico tecnológico universitario $amenity, iia), ///
	 ind($cmp_cont $cmp_cont $cmp_mprobit) 

	 *SUBEDUCADOS
margins, dydx(accesiblidad) expression(binormal( (predict(eq(#3))-predict(eq(#4)))/sqrt(2), (predict(eq(#3))-predict(eq(#5)))/sqrt(2), .5)) force
margins, dydx(acc) expression(binormal( (predict(eq(#3))-predict(eq(#4)))/sqrt(2), (predict(eq(#3))-predict(eq(#5)))/sqrt(2), .5)) force
	 
	 *SOBREDUCADOS
margins, dydx(accesiblidad) expression(binormal( (predict(eq(#5))-predict(eq(#3)))/sqrt(2), (predict(eq(#5))-predict(eq(#4)))/sqrt(2), .5)) force
margins, dydx(acc) expression(binormal( (predict(eq(#5))-predict(eq(#3)))/sqrt(2), (predict(eq(#5))-predict(eq(#4)))/sqrt(2), .5)) force

outreg2 using "regresion_cmp.xls"
