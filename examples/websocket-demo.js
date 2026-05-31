document.addEventListener('DOMContentLoaded', () => {
  const statusEl = document.getElementById('status');
  const messages = document.getElementById('messages');
  const form = document.getElementById('form');
  const input = document.getElementById('message');
  const ws = new WebSocket(`${location.protocol === 'https:' ? 'wss' : 'ws'}://${location.host}/ws`);

  function appendMessage(text, cls = 'bg-slate-800') {
    const row = document.createElement('div');
    row.className = `${cls} rounded-lg px-3 py-2`;
    row.textContent = text;
    messages.appendChild(row);
    messages.scrollTop = messages.scrollHeight;
  }

  ws.addEventListener('open', () => {
    statusEl.textContent = 'connected';
    statusEl.className = 'text-sm text-emerald-300';
  });

  ws.addEventListener('message', event => appendMessage(event.data));

  ws.addEventListener('close', () => {
    statusEl.textContent = 'closed';
    statusEl.className = 'text-sm text-rose-300';
  });

  form.addEventListener('submit', event => {
    event.preventDefault();
    const text = input.value.trim();
    if (!text) return;
    ws.send(text);
    input.value = '';
  });
});
