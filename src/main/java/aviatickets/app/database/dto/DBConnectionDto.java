package aviatickets.app.database.dto;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

public record DBConnectionDto(
		Connection connection,
		Statement statement,
		ResultSet resultSet
) {
}
