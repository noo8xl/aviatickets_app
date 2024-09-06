package aviatickets.app.database;

import aviatickets.app.database.dto.DBConnectionDto;

import java.sql.SQLException;

public interface DatabaseInterface {
// connect to db by chosen type of user
	DBConnectionDto initConnection(Byte type) throws ClassNotFoundException, SQLException;

// disconnect from the current db client instance
	void closeAndStopDBInteraction(DBConnectionDto dto) throws ClassNotFoundException, SQLException;

}
