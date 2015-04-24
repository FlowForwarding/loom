(function (angular) {
    'use strict';

    angular.module('nci.services.jsonUpload', [])
        .directive('nciFileUpload', function($state, $stateParams, modelPromise) {

            var template = '<span class="drag-file">Drop debug JSON here</span>';

            // Check for the various File API support.
            if (window.File && window.FileReader && window.FileList && window.Blob) {
                // Great success! All the File APIs are supported.
            } else {
                template = 'Input upload not supported in this browser';
            }

            function handleFileSelect(event) {

                var evt = event.originalEvent;

                evt.dataTransfer.dropEffect = 'none';

                evt.stopPropagation();
                evt.preventDefault();

                var files = evt.dataTransfer.files;

                readFile(files[0]);

            }

            function readFile(file) {
                // Only process json files.
                if (!file.type.match('json.*')) {
                    return alert("invalid file type");
                }

                var reader = new FileReader();

                // Closure to capture the file information.
                reader.onload = function(e) {
                    var result = JSON.parse(atob(e.target.result.replace("data:application/json;base64,", "")));

                    modelPromise.updateData(result);

                    $state.go($state.$current, $stateParams, {reload: true});

                    // Render thumbnail.
                    //var span = document.createElement('span');
                    //span.innerHTML = ['<img class="thumb" src="', e.target.result,
                    //    '" title="', escape(theFile.name), '"/>'].join('');
                    //document.getElementById('list').insertBefore(span, null);
                };

                // Read in the image file as a data URL.
                reader.readAsDataURL(file);
            }

            function handleDragOver(event) {
                event = event.originalEvent;
                event.stopPropagation();
                event.preventDefault();
                event.dataTransfer.dropEffect = 'copy'; // Explicitly show this is a copy.
            }



            // Setup the dnd listeners.
            return {
                restrict: "E",
                template: template,
                link: function($scope, $elem, $attrs) {
                    var dropZone = $elem;
                    dropZone.on('dragover', handleDragOver);
                    dropZone.on('dragenter', function() {
                        $elem.addClass("file-over");
                    });

                    dropZone.on('dragleave', function() {
                        $elem.removeClass("file-over");
                    });
                    dropZone.on('drop', handleFileSelect);

                }
            };
        });

})(angular);
