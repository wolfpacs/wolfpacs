import unittest
from mini_admin import WolfPACS


class TestWolfPACSClass(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.wolfpacs = WolfPACS()

    def test_add_clients(self):
        self.wolfpacs.add_client("c1", "ae_c1")
        self.wolfpacs.add_client("c2", "ae_c2")
        clients = self.wolfpacs.clients()
        self.assertEqual(len(clients), 2)

    def test_add_workers(self):
        self.wolfpacs.add_worker("w1", "localhost", "1111", "ae_w1")
        self.wolfpacs.add_worker("w2", "localhost", "2222", "ae_w2")
        self.wolfpacs.add_worker("w3", "localhost", "3333", "ae_w3")
        workers = self.wolfpacs.workers()
        self.assertEqual(len(workers), 3)

    def test_add_destinations(self):
        self.wolfpacs.add_destination("d1", "localhost", "4444", "ae_d1")
        self.wolfpacs.add_destination("d2", "localhost", "5555", "ae_d2")
        self.wolfpacs.add_destination("d3", "localhost", "6666", "ae_d3")
        destinations = self.wolfpacs.destinations()
        self.assertEqual(len(destinations), 3)

if __name__ == '__main__':
    unittest.main()
