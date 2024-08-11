package aviatickets.app.jwt;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import java.security.Key;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

@NoArgsConstructor
@Service
public class JwtService implements JwtInterface {

	@Value("${spring.datasource.jwt-secrete-key}")
	private String secreteKey;

	@Override
	public String extractCustomerEmail(String token) {
		return this.getClaim(token, Claims::getSubject);
	}

	@Override
	public String generateToken(UserDetails userDetails) {
		return this.generateToken(new HashMap<>(), userDetails);
	}

	@Override
	public Boolean isTokenValid(String token, UserDetails userDetails) {
		final String userEmail = this.getClaim(token, Claims::getSubject);
		return userEmail.equals(userDetails.getUsername()) && !this.isTokenExpired(token);
	}

	@Override
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
		return this.getClaim(token, Claims::getExpiration);
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
				.signWith(this.getSignInKey(), SignatureAlgorithm.HS256)
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
		byte[] keyBytes = Decoders.BASE64.decode(this.secreteKey);
		return Keys.hmacShaKeyFor(keyBytes);
	}

}
