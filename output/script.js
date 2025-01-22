function preventBrowserDefault(e) {
    if (e.cancelable) {
        e.preventDefault();
        __PS_MV_REG = [];
        return e.stopPropagation();
    };
};
function performSearch(query) {
    if (query && query.length > 0) {
        var url = 'https://startpage.com/search?q=' + window.encodeURIComponent(query);
        __PS_MV_REG = [];
        return window.location.href = url;
    };
};
function isInSearch() {
    return document.activeElement.id == 'search-input';
};
function clearAndBlurSearch() {
    var search = document.getElementById('search-input');
    if (search) {
        search.value = '';
        __PS_MV_REG = [];
        return search.blur();
    };
};
function handleKey(e) {
    var isCtrl = e.ctrlKey;
    var key15 = e.key;
    if (isCtrl && key15 === 's') {
        preventBrowserDefault(e);
        var search = document.getElementById('search-input');
        search.focus();
        __PS_MV_REG = [];
        return search.select();
    } else if (isCtrl && key15 === 'g') {
        preventBrowserDefault(e);
        __PS_MV_REG = [];
        return clearAndBlurSearch();
    };
};
function handleSearchSubmit(e) {
    preventBrowserDefault(e);
    var input = document.getElementById('search-input');
    var query = input && input.value;
    __PS_MV_REG = [];
    return performSearch(query);
};
function handleSearchKeydown(e) {
    var key16 = e.key;
    if (key16 === 'Enter') {
        var query = e.target.value;
        preventBrowserDefault(e);
        __PS_MV_REG = [];
        return performSearch(query);
    } else if (e.ctrlKey && key16 === 'g') {
        __PS_MV_REG = [];
        return e.target.blur();
    };
};
function init() {
    var favicon = document.createElement('link');
    favicon.rel = 'icon';
    favicon.type = 'image/x-icon';
    favicon.href = 'favicon.png';
    document.head.appendChild(favicon);
    var form = document.getElementById('search-form');
    var input = document.getElementById('search-input');
    if (input) {
        input.onkeydown = function (e) {
            __PS_MV_REG = [];
            return handleSearchKeydown(e);
        };
    };
    if (form) {
        form.onsubmit = function (e) {
            __PS_MV_REG = [];
            return handleSearchSubmit(e);
        };
    };
    __PS_MV_REG = [];
    return document.onkeydown = handleKey;
};
window.onload = init;