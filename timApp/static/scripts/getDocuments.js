
    var documents ="" ;
    $.ajax({
        url: "/getDocuments/",
        dataType: 'json',
        async: false,
        success: function(data) {
            documents = data;
        }
    });

