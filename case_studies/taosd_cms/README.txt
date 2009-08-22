 
## MessageBus service

  - msgBus::send(Channel, Info, Entity)
send on channel 'Channel' that an information 'Info' concerns 'Entity'

  - msgBus::wait4msg(Channel, Info, Entity)
wait for an information 'Info' concerning 'Entity' on channel 'Channel')

## UserInterface service

  - ui::actListener(Employee, Entity, Channel)
Open on 'Employee' HCI a window which logs everythong broadcasted on channel 
'Channel' concerning 'Entity'. Do nothing is such a window exists;

  - ui::stopListener(Employee, Entity, Channel)
Close on 'Employee' HCI the log window concerning 'Channel' and 'Entity'. Do
nothing if such a windows doesn't exists.
