var currentLink = -1;
function getAllLinks() {
    __PS_MV_REG = [];
    return document.querySelectorAll('.link');
};
function focusLink(index) {
    var links = getAllLinks();
    var total = links.length;
    var newIndex = index < 0 ? total - 1 : (index >= total ? 0 : index);
    if (total > 0) {
        if (currentLink >= 0 && currentLink < total) {
            links[currentLink].removeAttribute('data-focused');
        };
        currentLink = newIndex;
        var current = links[newIndex];
        current.setAttribute('data-focused', 'true');
        __PS_MV_REG = [];
        return current.scrollIntoView({ behavior : 'smooth', block : 'center' });
    };
};
function isInSearch() {
    return document.activeElement.id == 'search-input';
};
function preventBrowserDefault(e) {
    if (e.cancelable) {
        e.preventDefault();
        __PS_MV_REG = [];
        return e.stopPropagation();
    };
};
function handleKey(e) {
    var isCtrl = e.ctrlKey;
    var key19 = e.key;
    if (isCtrl && key19 === 'n') {
        e.preventDefault();
        e.stopPropagation();
        __PS_MV_REG = [];
        return focusLink(currentLink + 1);
    } else {
        if (!(isInSearch() && !(isCtrl && (key19 === 'g' || key19 === 's')))) {
            if (isCtrl && key19 === 'p') {
                e.preventDefault();
                __PS_MV_REG = [];
                return focusLink(currentLink - 1);
            } else if (isCtrl && key19 === 's') {
                e.preventDefault();
                var search = document.getElementById('search-input');
                search.focus();
                __PS_MV_REG = [];
                return search.select();
            } else if (isCtrl && key19 === 'm') {
                e.preventDefault();
                if (currentLink >= 0) {
                    var links = getAllLinks();
                    var current = links[currentLink];
                    __PS_MV_REG = [];
                    return window.location.href = current.href;
                };
            } else if (isCtrl && key19 === 'a') {
                e.preventDefault();
                __PS_MV_REG = [];
                return focusLink(0);
            } else if (isCtrl && key19 === 'e') {
                e.preventDefault();
                __PS_MV_REG = [];
                return focusLink(getAllLinks().length - 1);
            } else if (isCtrl && key19 === 'g') {
                e.preventDefault();
                var search20 = document.getElementById('search-input');
                search20.blur();
                currentLink = -1;
                __PS_MV_REG = [];
                return getAllLinks().forEach(function (link) {
                    __PS_MV_REG = [];
                    return link.removeAttribute('data-focused');
                });
            };
        };
    };
};
function init() {
    var input = document.getElementById('search-input');
    if (input) {
        input.onkeydown = function (e) {
            if (e.ctrlKey && e.key === 'n') {
                e.preventDefault();
                __PS_MV_REG = [];
                return e.stopPropagation();
            } else if (e.ctrlKey && e.key === 'g') {
                __PS_MV_REG = [];
                return input.blur();
            } else if (e.key === 'Enter') {
                var query = input.value;
                if (query.length > 0) {
                    e.preventDefault();
                    __PS_MV_REG = [];
                    return window.location.href = 'https://startpage.com/search?q=' + URI.encode(query);
                };
            };
        };
    };
    __PS_MV_REG = [];
    return document.onkeydown = handleKey;
};
window.onload = init;