ePubSub
=====

Erlang Publisher Subscriber using ActiveMQ Broker.
* This project makes use of STOMP.
* ePubSub can be run as a standalone application with 1 process created as publisher and 2 processes created as subscribers 
  each performing an aggregate function assigned to them (Sum & Median respectively).
  
* This application also can be run as 2 separate applications(based on the "run_as" parameter). 
  i.e. it can be run as either publisher or subscriber mode. 
  
* Publisher will generate random integers from the configured <b>range</b> and will publish to the <b>topic</b> 
  configured.
  
* Subscribers will listen for incoming messages on the topic and will perform aggregate functions at regular
intervals.
  
* HTTP APIs (using cowboy) are implemented for retrieval of Aggregate function results and for the list of numbers on 
  which the function executed. 

### <u> Dependency </u>
* stomp client written in erlang (placed under epsUtils/stomp.erl) is used for communicating with ActiveMQ server. 
  The original opensource file has been patched for usage with ePubSub. 

Environment.
-----
* Ubuntu 20.04
* ActiveMQ.
* For compiling, Erlang & rebar3 need to be installed. Please see the setup instructions below.

Setup Instructions.
-----
### <u>ActiveMQ Installation </u>
    $ sudo apt update && sudo apt-get install default-jre
    $ cd /tmp && wget http://archive.apache.org/dist/activemq/5.15.8/apache-activemq-5.15.8-bin.tar.gz
    $ tar -xvzf apache-activemq-5.15.8-bin.tar.gz && sudo mv apache-activemq-5.15.8 /opt/activemq
    $ sudo addgroup --quiet --system activemq 
    $ sudo adduser --quiet --system --ingroup activemq --no-create-home --disabled-password activemq
    $ sudo chown -R activemq:activemq /opt/activemq
    
    Edit the below file and add the contents as mentioned
    $ sudo vi /etc/systemd/system/activemq.service
        [Unit]
        Description=Apache ActiveMQ
        After=network.target
        [Service]
        Type=forking
        User=activemq
        Group=activemq
        
        ExecStart=/opt/activemq/bin/activemq start
        ExecStop=/opt/activemq/bin/activemq stop
        
        [Install]
        WantedBy=multi-user.target
    
    After saving the file.
    $ sudo systemctl daemon-reload
    $ sudo systemctl start activemq
    $ sudo systemctl enable activemq
    
    Verify ActiveMQ is running.
    $ /opt/activemq/bin/activemq status


### <u>Erlang / OTP & rebar3 Installation </u>
<b>Note</b>: This step is required only if the source code needs to be compiled.

    $ sudo wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -
    $ echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" | sudo tee /etc/apt/sources.list.d/rabbitmq.list
    $ sudo apt update && sudo apt install erlang

    $ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
    $ sudo cp -rf rebar3 /usr/bin/

Build
-----
    $ rebar3 release

* config & binary files will be present in ./_build/default/rel/ePubSub/.
* Binary file (ePubSub) will be present @ bin/ folder and can be run as ./ePubSub daemon.
* Logs will be generated @ logs/ folder with name error.\<num\>.
* Application specific configuration is present @ "releases/\<release_version\>/sys.config".
* Configurable parameters are as follows.
  
      {ePubSub, [
          {run_as, "both"},                       <-- One of "publisher", "subscriber" or "both".
          {amq_server, "localhost"},              <-- ActiveMQ Server to connect.
          {amq_server_port, 61613},               <-- ActiveMQ Server connection port.
          {publish_rate, 20},                     <-- Rate @ which publisher should start publish the messages.
          {publish_range, 100},                   <-- Range from which random integers are used for publishing.
          {eps_topic, "/topic/SampleTopic"},      <-- ActiveMQ topic to which messages are published.
          {subs_sleep_intvl, 5000}                <-- Interval @ which subscriber needs to perform Aggregate function.
          ]},
* Application can be started as <b> ./ePubSub daemon </b>
* To stop the application: <b> ./ePubSub stop </b>

Release Tar ball
-----
    $ rebar3 as prod tar

Above command will generate a tarball with signature "ePubSub-0.1.0.tar.gz" and can be used for deployments.
Same application start & stop commands can be used for operating the executable.

Run as publisher
-----
* copy the release tar ball to a folder, untar and change configuration files (sys.config & vm.args), as shown below.
  
  cat ./releases/0.1.0/sys.config

        {ePubSub, [
          {run_as, "publisher"},
          {amq_server, "localhost"},
          {amq_server_port, 61613},
          {publish_rate, 20},
          ....
          ....

  cat ./releases/0.1.0/vm.args


    ##-sname ePubSub
    ## Use one of the below options if running as publisher only or subscriber only.
    -sname ePubSub_pub
    ##-sname ePubSub_sub
    -setcookie ePubSub_cookie
    +K true
    +A30


Run as subscriber
-----
* copy the release tar ball to a folder, untar and change configuration files (sys.config & vm.args), as shown below.

  cat ./releases/0.1.0/sys.config

        {ePubSub, [
          {run_as, "subscriber"},
          {amq_server, "localhost"},
          {amq_server_port, 61613},
          {publish_rate, 20},
          ....
          ....

  cat ./releases/0.1.0/vm.args


    ##-sname ePubSub
    ## Use one of the below options if running as publisher only or subscriber only.
    ##-sname ePubSub_pub
    -sname ePubSub_sub
    -setcookie ePubSub_cookie
    +K true
    +A30

Run as both Publisher and Subscriber
-----
* copy the release tar ball to a folder, untar and change configuration files (sys.config & vm.args), as shown below.

  cat ./releases/0.1.0/sys.config

        {ePubSub, [
          {run_as, "both"},
          {amq_server, "localhost"},
          {amq_server_port, 61613},
          {publish_rate, 20},
          ....
          ....

  cat ./releases/0.1.0/vm.args


    -sname ePubSub
    ## Use one of the below options if running as publisher only or subscriber only.
    ##-sname ePubSub_pub
    ##-sname ePubSub_sub
    -setcookie ePubSub_cookie
    +K true
    +A30

Supported HTTP APIs
-----
* <b>Endpoint URL</b>: http://<host_where_subscriber_runs>:8080/<subscriber_name>/<get_type>
* <b>Accept Type</b>: "application/json"
* Valid Values for <b>subscriber_name</b>: "subscriber_1" or "subscriber_2"
* Valid Values for <b>get_type</b>: "aggr_result" (for retrieving aggregate result) 
  or "aggr_list" (for retrieving the list on which aggregate function acted upon.)
  
* <b>NOTE:</b> HTTP APIs will be active only on when the run_mode is either "both" / "subscriber".

#### Samples
    $ curl -H "Accept: application/json" http://localhost:8080/subscriber_1/aggr_result
    2004

    $ curl -H "Accept: application/json" http://localhost:8080/subscriber_1/aggr_list
    [87,36,76,72,55,20,17,64,66,22,87,38,7,63,84,12,47,61,89,41,44,37,36,59,52,80,94,53,97,91,22,16,70,60,87,32,20,60,
     73,20,23,77,5,90,14,15,3,45,25,44,6,63,84,6,66,88,57,58,44,17,43,72,61,88,34,48,52,63,32,63,24,53,76,25,18,13,95,57,44,9,85,32,51,74,3,62,96,95,80,43,89,90,68,35,86,78,77,24,90]

    $ curl -H "Accept: application/json" http://localhost:8080/subscriber_2/aggr_list
    [83,17,82,6,49,42,77,17,75,98,25,22,41,58,10,21,85,86,71,30,50,11,80,90,48,24,56,29,95,29,64,16,71,91,6,40,54,82,59,
     41,11,42,87,79,56,81,8,64,38,57,97,98,31,11,91,19,82,35,57,94,25,4,90,48,77,40,17,25,82,79,52,48,65,86,9,81,36,10,68,75,8,79,11,16,68,68,41,92,42,35,30,8,40,63,3,92,2,73,74]

    $ curl -H "Accept: application/json" http://localhost:8080/subscriber_2/aggr_result
    4.80000000000000000000e+01

    $ curl -H "Accept: application/json" http://localhost:8080/subscriber_3/aggr_result
    "This subscriber is not present"

Enable Debug log
-----
    Modify following lines in sys.config

      {logger_level, info}         ==>  {logger_level, debug},

      level => info,               ==>  level => debug,
#### Publisher Output (logs/error.*)
    debug: Sending message...39
    debug: Sending message...91
    debug: Sending message...3
    debug: Sending message...31
    debug: Sending message...72
    debug: Sending message...98
    debug: Sending message...90
    debug: Sending message...34


#### Subscriber Output (logs/error.*)
    debug: Incoming messages for subscriber_1 over the subscription time: [33,40,93,38,81,6,50,51,71,71,2,52,41,87,50,49,25,96,52,81,90,76,43,60,74,20,78,7,2,38,48,79,73,54,35,41,11,29,2]
    debug: Output from subscriber_1 Aggregated Function(calculate_sum) is: 1929
    debug: Incoming messages for subscriber_2 over the subscription time: [33,40,93,38,81,6,50,51,71,71,2,52,41,87,50,49,25,96,52,81,90,76,43,60,74,20,78,7,2,38,48,79,73,54,35,41,11,29,2]
    debug: Output from subscriber_2 Aggregated Function(calculate_median) is: 50.0

Future Improvements
-----
* Run the application in a docker environment.
* Update Restful APIs to support text, html formats.