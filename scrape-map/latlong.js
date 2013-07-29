//http://mapas.comipems.org.mx/

console.log('muncode, statecode, school, name, inst, modalidad, especialidad, long, lat, minscore2010, minscore2011, minscore2012');
$('#opcionesEducativas tbody tr').each(function(d,i){
    laChkBox = $('input:checkbox', this).attr('id'); 
    //console.log($('#'+laChkBox).dataset('name');
    console.log($('#'+laChkBox).data('municipio') + ',' + $('#'+laChkBox).data('entidad') +',' +$('#'+laChkBox).data('opcioneducativa') + ',"' + $('#'+laChkBox).data('name') + '",' + $('#'+laChkBox).data('institucion') + ',' + $('#'+laChkBox).data('modalidad') + ',"' + $('#'+laChkBox).data('especialidad') + '",' +$('#'+laChkBox).data('longitude') + ',' + $('#'+laChkBox).data('latitude') + ',' +$('#'+laChkBox).data('hace2anios') + ',' +$('#'+laChkBox).data('hace1anios') + ',' +$('#'+laChkBox).data('hace0anios')+'\n')
})


//    + ',"' + $('#'+laChkBox).data('name') + '","' +$('#'+laChkBox).data('institucion') + '",' +$('#'+laChkBox).data('modalidad') + ',' +$('#'+laChkBox).data('municipio') + ',' +$('#'+laChkBox).data('entidad') + ',' +$('#'+laChkBox).data('hace3anios') + ',' +$('#'+laChkBox).data('hace2anios') + ',' +$('#'+laChkBox).data('hace1anios') + ',' +$('#'+laChkBox).data('hace0anios')  
