-record(relay, {process_name,
				node_name		}).

-record(payload_request, {direction,
						  src_node_name,
						  rcv_process_name}).