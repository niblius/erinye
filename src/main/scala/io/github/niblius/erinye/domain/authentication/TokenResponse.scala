package io.github.niblius.erinye.domain.authentication

import java.time.Instant

case class TokenResponse(token: String, expiry: Instant)
