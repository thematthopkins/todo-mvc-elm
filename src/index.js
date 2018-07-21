import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var localStorageKey = 'todo_items';
var loaded = JSON.parse(localStorage.getItem(localStorageKey));

if (loaded == null) {
    loaded = {
        nextID: 0,
        items: []
    };
}

var app = Main.embed(
    document.getElementById('root'),
    { 'items': loaded });


app.ports.persistList.subscribe(function (items) {
    localStorage.setItem(localStorageKey, JSON.stringify(items));
});

registerServiceWorker();
