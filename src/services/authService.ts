import { LoginRequest, LoginResponse, AuthUser } from '@/types/auth';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL || '/api';

class AuthService {
  private getHeaders(): Record<string, string> {
    return {
      'Content-Type': 'application/json',
    };
  }

  async login(userId: string, password: string): Promise<LoginResponse> {
    const requestBody: LoginRequest = {
      userId,
      password,
    };

    const response = await fetch(`${API_BASE_URL}/auth/login`, {
      method: 'POST',
      headers: this.getHeaders(),
      body: JSON.stringify(requestBody),
    });

    if (response.status === 400) {
      const errorData = await response.json();
      throw new Error(errorData.message || 'Wrong Password. Try again...');
    }

    if (!response.ok) {
      throw new Error('Authentication failed');
    }

    const data: LoginResponse = await response.json();
    
    this.setAuthToken(data.userId);
    this.setAuthUser({
      userId: data.userId,
      firstName: data.firstName,
      lastName: data.lastName,
      userType: data.userType,
    });

    return data;
  }

  logout(): void {
    this.removeAuthToken();
    this.removeAuthUser();
  }

  setAuthToken(token: string): void {
    if (typeof window !== 'undefined') {
      localStorage.setItem('auth_token', token);
    }
  }

  getAuthToken(): string | null {
    if (typeof window !== 'undefined') {
      return localStorage.getItem('auth_token');
    }
    return null;
  }

  removeAuthToken(): void {
    if (typeof window !== 'undefined') {
      localStorage.removeItem('auth_token');
    }
  }

  setAuthUser(user: AuthUser): void {
    if (typeof window !== 'undefined') {
      localStorage.setItem('auth_user', JSON.stringify(user));
    }
  }

  getAuthUser(): AuthUser | null {
    if (typeof window !== 'undefined') {
      const userStr = localStorage.getItem('auth_user');
      if (userStr) {
        try {
          return JSON.parse(userStr) as AuthUser;
        } catch (error) {
          console.error('Failed to parse auth user:', error);
          return null;
        }
      }
    }
    return null;
  }

  removeAuthUser(): void {
    if (typeof window !== 'undefined') {
      localStorage.removeItem('auth_user');
    }
  }

  isAuthenticated(): boolean {
    return this.getAuthToken() !== null && this.getAuthUser() !== null;
  }

  getCurrentUser(): AuthUser | null {
    return this.getAuthUser();
  }

  getUserType(): 'A' | 'R' | null {
    const user = this.getAuthUser();
    return user ? user.userType : null;
  }

  isAdmin(): boolean {
    return this.getUserType() === 'A';
  }

  isRegularUser(): boolean {
    return this.getUserType() === 'R';
  }
}

export const authService = new AuthService();
