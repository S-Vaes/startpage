function init() {
    var input = document.getElementById('search-input');
    if (input) {
        document.onkeydown = function (e) {
            if (e.key === '/' && document.activeElement != input) {
                e.preventDefault();
                return input.focus();
            };
        };
        return input.onkeydown = function (e) {
            if (e.key === 'Escape') {
                return input.blur();
            } else if (e.key === 'Enter') {
                var query = input.value;
                if (query.length > 0) {
                    e.preventDefault();
                    __PS_MV_REG = [];
                    return window.location.href = 'https://startpage.com/search?q=' + encodeURI(query);
                };
            };
        };
    };
};
window.onload = init;