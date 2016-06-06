import diamond.collector
import subprocess


class TestCollector(diamond.collector.Collector):
    def get_default_config_help(self):
        config_help = super(ExampleCollector, self).get_default_config_help()
        config_help.update({
        })
        return config_help
        
    def get_default_config(self):
        config = super(TestCollector, self).get_default_config()
        config.update({
            'path':     'test',
        })
        return config
        
    def collect(self):
        # Set Metric Name
        metric_name_1 = "server.count.metric"
        metric_name_2 = "client.count.metric"
        # Set Metric Value
        metric_value_1 = subprocess.check_output("netstat -tn --tcp | grep ESTABLISHED | grep \"10.100.0.99\" | wc -l", shell=True)
        metric_value_2 = subprocess.check_output("netstat -tn --tcp | grep ESTABLISHED | grep \":3911\" | wc -l", shell=True)
        # Publish Metric
        self.publish(metric_name_1, metric_value_1)
        self.publish(metric_name_2, metric_value_2)