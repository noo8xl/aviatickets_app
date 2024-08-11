package aviatickets.app.database;

import aviatickets.app.database.dto.DBConnectionDto;

import java.sql.SQLException;

public interface DatabaseInterface {

	DBConnectionDto initConnection(Byte type) throws ClassNotFoundException, SQLException;

	void closeAndStopDBInteraction(DBConnectionDto dto) throws ClassNotFoundException, SQLException;

}
