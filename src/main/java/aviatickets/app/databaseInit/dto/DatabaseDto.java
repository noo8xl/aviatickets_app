package aviatickets.app.databaseInit.dto;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

public record DatabaseDto(
		Connection connection,
		Statement statement,
		ResultSet resultSet
) {
}
