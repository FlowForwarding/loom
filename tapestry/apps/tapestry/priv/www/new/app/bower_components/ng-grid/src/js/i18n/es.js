﻿(function () {
  angular.module('ui.grid').config(['$provide', function($provide) {
    $provide.decorator('i18nService', ['$delegate', function($delegate) {
      $delegate.add('es', {
        aggregate: {
          label: 'Artículos'
        },
        groupPanel: {
          description: 'Arrastre un encabezado de columna aquí y suéltelo para agrupar por esa columna.'
        },
        search: {
          placeholder: 'Buscar...',
          showingItems: 'Artículos Mostrados:',
          selectedItems: 'Artículos Seleccionados:',
          totalItems: 'Artículos Totales:',
          size: 'Tamaño de Página:',
          first: 'Primera Página',
          next: 'Página Siguiente',
          previous: 'Página Anterior',
          last: 'Última Página'
        },
        menu: {
          text: 'Elegir columnas:'
        },
        sort: {
          ascending: 'Orden Ascendente',
          descending: 'Orden Descendente',
          remove: 'Sin Ordenar'
        },
        column: {
          hide: 'Ocultar la columna'
        },
        aggregation: {
          count: 'filas totales: ',
          sum: 'total: ',
          avg: 'media: ',
          min: 'min: ',
          max: 'max: '
        },
        pinning: {
          pinLeft: 'Fijar a la Izquierda',
          pinRight: 'Fijar a la Derecha',
          unpin: 'Quitar Fijación'
        },
        gridMenu: {
          columns: 'Columnas:',
          importerTitle: 'Importar archivo',
          exporterAllAsCsv: 'Exportar todo como csv',
          exporterVisibleAsCsv: 'Exportar vista como csv',
          exporterSelectedAsCsv: 'Exportar selección como csv',
          exporterAllAsPdf: 'Exportar todo como pdf',
          exporterVisibleAsPdf: 'Exportar vista como pdf',
          exporterSelectedAsPdf: 'Exportar selección como pdf'
        },
        importer: {
          noHeaders: 'No fue posible derivar los nombres de las columnas, ¿tiene encabezados el archivo?',
          noObjects: 'No fue posible obtener registros, ¿contiene datos el archivo, aparte de los encabezados?',
          invalidCsv: 'No fue posible procesar el archivo, ¿es un CSV válido?',
          invalidJson: 'No fue posible procesar el archivo, ¿es un Json válido?',
          jsonNotArray: 'El archivo json importado debe contener un array, abortando.'
        }
      });
      return $delegate;
    }]);
}]);
})();
