/*global Meteor, Router, Template, Session, $*/

/////////////////
// Shared code //
/////////////////
var Users, Rooms, Messages

Users = new Meteor.Collection("users");
Rooms = new Meteor.Collection("rooms");
Messages = new Meteor.Collection("messages");

//////////////////////
// Client side code //
//////////////////////
if (Meteor.isClient) {
  Router.configure({
    layoutTemplate: "layout"
  });
  Router.map(function() {
    this.route('home', {
      path: '/',
      onBeforeAction: function() {
        Meteor.subscribe("roomSub");
        Meteor.subscribe("allUserSub");
      },
      data: function() {
        return {
          rooms: Rooms.find({})
        }
      }
    });
    this.route('chatRoom', {
      path: '/chat/:room',
      onBeforeAction: function() {
        Meteor.subscribe("messageSub", this.params.room);
        Meteor.subscribe("userSub", this.params.room);
        Meteor.subscribe("roomSub");
      },
      data: function() {
        return {
          room: this.params.room,
          username: null,
          users: Users.find({room: this.params.room}),
          messages: Messages.find(
            {room: this.params.room},
            {sort: {timestamp: 1}}
          )
        }
      }
    });
  });

  Template.home.userCount = function(room) {
    return Users.find({room: room}).count()
  }

  Template.home.events({
    'submit': function (evt) {
      evt.preventDefault();
      var $input = $(evt.target).find("input");
      Meteor.call("addRoom", $input.val());
      $input.val("");
    }
  });

  Template.chatRoom.events({
    'submit': function (evt) {
      evt.preventDefault();
      var $input = $(evt.target).find("input");
      if (!this.username) {
        this.username = $input.val();
        Users.insert({
          name: this.username,
          room: this.room
        });
        Messages.insert({
          user: this.username,
          broadcast: true,
          timestamp: Date.now(),
          content: this.username + " has joined the channel",
          room: this.room
        });
        Meteor.call("addRoom", this.room);
        $("#chatBody div").last().before("<div><em>Welcome, " + this.username + "</em></div>")
      } else {
        Messages.insert({
          user: this.username,
          broadcast: false,
          timestamp: Date.now(),
          content: $input.val(),
          room: this.room
        });
      }
      $input.val("");
    }
  });
}


//////////////////////
// Server side code //
//////////////////////
if (Meteor.isServer) {
  Meteor.publish("userSub", function(room) {
    return Users.find({ room : room });
  });
  Meteor.publish("allUserSub", function() {
    return Users.find({});
  });
  Meteor.publish("roomSub", function() {
    return Rooms.find();
  });
  Meteor.publish("messageSub", function(room) {
    return Messages.find({ room : room });
  });

  Meteor.methods({
    addRoom: function(name) {
      var room = Rooms.findOne({
        name: name
      });
      if (!room) {
        Rooms.insert({
          name: name
        });
      }
    }
  });

  Meteor.startup(function() {
    Users.remove({});
    Messages.remove({});
    Rooms.remove({});
  });
}
