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

### <u> Dependency </u>
* stomp client written in erlang (placed under epsUtils/stomp.erl) is used for communicating with ActiveMQ server. 
  The original source file has been patched for usage with ePubSub. 

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

Publisher Output (logs/error.*)
-----
    info: Sending message...39
    info: Sending message...91
    info: Sending message...3
    info: Sending message...31
    info: Sending message...72
    info: Sending message...98
    info: Sending message...90
    info: Sending message...34


Subscriber Output (logs/error.*)
-----
    info: Incoming messages for subscriber_1 over the subscription time: [33,40,93,38,81,6,50,51,71,71,2,52,41,87,50,49,25,96,52,81,90,76,43,60,74,20,78,7,2,38,48,79,73,54,35,41,11,29,2]
    info: Output from subscriber_1 Aggregated Function(calculate_sum) is: 1929
    info: Incoming messages for subscriber_2 over the subscription time: [33,40,93,38,81,6,50,51,71,71,2,52,41,87,50,49,25,96,52,81,90,76,43,60,74,20,78,7,2,38,48,79,73,54,35,41,11,29,2]
    info: Output from subscriber_2 Aggregated Function(calculate_median) is: 50.0

### Future Improvements
