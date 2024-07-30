package aviatickets.app.jwt;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import java.security.Key;
import java.sql.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

@Service
public class JwtService {

	@Value("${spring.datasource.jwt-secrete-key}")
	private static String SECRET_KEY;

	public String extractCustomerEmail(String token) {
		return this.getClaim(token, Claims::getSubject);
	}

	public String generateToken(UserDetails userDetails) {
		return this.generateToken(new HashMap<>(), userDetails);
	}

	public Boolean isTokenValid(String token, UserDetails userDetails) {
		final String userEmail = this.getClaim(token, Claims::getSubject);
		return userEmail.equals(userDetails.getUsername()) && !this.isTokenExpired(token);
	}

	public <T> T getClaim(String token, Function<Claims, T> claimsResolver) {
		final Claims claims = this.getClaimsFromToken(token);
		return claimsResolver.apply(claims);
	}

	// ###########################################################################################
	// ###########################################################################################
	// ###########################################################################################

	private Boolean isTokenExpired(String token) {
		return this.extractExpiration(token).before(new Date(System.currentTimeMillis()));
	}

		private Date extractExpiration(String token) {
			return (Date) this.getClaim(token, Claims::getExpiration);
		}

	private String generateToken(
		Map<String, Object> claims,
		UserDetails userDetails
	) {
		return Jwts
				.builder()
				.setClaims(claims)
				.setSubject(userDetails.getUsername())
				.setIssuedAt(new Date(System.currentTimeMillis()))
				.setExpiration(new Date(System.currentTimeMillis() + 1000 * 60 * 24))
				.signWith(getSignInKey(), SignatureAlgorithm.ES256)
				.compact();
	}

	private Claims getClaimsFromToken(String token) {
		return Jwts
				.parserBuilder()
				.setSigningKey(this.getSignInKey())
				.build()
				.parseClaimsJws(token)
				.getBody();
	}

	private Key getSignInKey() {
		byte[] keyBytes = Decoders.BASE64.decode(SECRET_KEY);
		return Keys.hmacShaKeyFor(keyBytes);
	}

}
