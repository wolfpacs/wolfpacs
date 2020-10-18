import requests
import click


class WolfPACS:
    def __init__(self, host='localhost', port=8080, proto='http'):
        self.host = host
        self.port = port
        self.proto = proto

    def _get(self, endpoint):
        url = f'{self.proto}://{self.host}:{self.port}/{endpoint}'
        res = requests.get(url)
        return res.json()

    def _post(self, endpoint, payload):
        url = f'{self.proto}://{self.host}:{self.port}/{endpoint}'
        res = requests.post(url, json=payload)
        return res.json()

    def add_client(self, name, ae):
        payload = {"name": name,
                   "ae": ae}
        self._post('clients', payload)

    def add_destination(self, name, host, port, ae):
        payload = {"name": name,
                   "host": host,
                   "port": port,
                   "ae": ae}
        self._post('dests', payload)

    def add_worker(self, name, host, port, ae):
        payload = {"name": name,
                   "host": host,
                   "port": port,
                   "ae": ae}
        self._post('workers', payload)

    def assoc_worker(self, client_name, worker_name):
        payload = {"name": worker_name}
        self._post(f'clients/{client_name}/workers', payload)

    def assoc_destination(self, client_name, destination_name):
        payload = {"name": destination_name}
        self._post(f'clients/{client_name}/dests', payload)

    def clients(self):
        return self._get('clients')

    def destinations(self):
        return self._get('dests')

    def workers(self):
        return self._get('workers')

    def client_workers(self, client_name):
        return self._get(f'clients/{client_name}/workers')

    def client_dests(self, client_name):
        return self._get(f'clients/{client_name}/dests')


@click.command()
@click.option('--wolfpacs-host', default='localhost', help='WolfPACS hostname')
@click.option('--wolfpacs-port', default='8080', help='WolfPACS admin port')
@click.option('--client', default='', help='client name (optional)')
@click.option('--name', default='', help='name (optional)')
@click.option('--host', default='', help='hostname (optional')
@click.option('--port', default='', help='port (optional')
@click.option('--ae', default='', help='AE (optional')
@click.option('--add-client', default=False, is_flag=True, help='Add a client')
@click.option('--add-worker', default=False, is_flag=True, help='Add a worker')
@click.option('--add-dest', default=False, is_flag=True, help='Add a destination')
@click.option('--assoc-worker', default=False, is_flag=True, help='Associate a worker with a client')
@click.option('--assoc-dest', default=False, is_flag=True, help='Associate a destination with a client')
def main(wolfpacs_host, wolfpacs_port,
         client,
         name, host, port, ae,
         add_client, add_worker, add_dest,
         assoc_worker, assoc_dest):
    wolfpacs = WolfPACS(wolfpacs_host, wolfpacs_port)

    if add_client:
        wolfpacs.add_client(name, ae)
    elif add_worker:
        wolfpacs.add_worker(name, host, port, ae)
    elif add_dest:
        wolfpacs.add_destination(name, host, port, ae)
    elif assoc_worker:
        wolfpacs.assoc_worker(client, name)
    elif assoc_dest:
        wolfpacs.assoc_destination(client, name)
    else:
        print('No task given')

if __name__ == '__main__':
    main()
