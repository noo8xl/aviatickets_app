package aviatickets.app.util.entity;

import aviatickets.app.actions.entity.ActionLog;

import java.util.List;

public record Actions(List<ActionLog> actions) {
}
