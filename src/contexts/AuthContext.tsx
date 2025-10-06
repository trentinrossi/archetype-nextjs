'use client';

import React, { createContext, useContext, useState, useEffect } from 'react';
import { authService } from '@/services/authService';
import { UserResponse, UserCreate } from '@/types/auth';

interface AuthContextType {
  isAuthenticated: boolean;
  user: UserResponse | null;
  loading: boolean;
  login: (username: string, password: string) => Promise<boolean>;
  register: (userData: UserCreate) => Promise<boolean>;
  logout: () => void;
  refreshToken: () => Promise<boolean>;
}

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [user, setUser] = useState<UserResponse | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const initializeAuth = async () => {
      const token = authService.getToken();
      const savedUserData = authService.getUser();

      if (token && savedUserData) {
        try {
          // Verify token is still valid by fetching current user
          const currentUser = await authService.getCurrentUser();
          setUser(currentUser);
          setIsAuthenticated(true);
          authService.setUser(currentUser); // Update stored user data
        } catch (error) {
          // Token is invalid, clear stored data
          console.error('Token validation failed:', error);
          authService.removeToken();
          setUser(null);
          setIsAuthenticated(false);
        }
      }
      setLoading(false);
    };

    initializeAuth();
  }, []);

  const login = async (username: string, password: string): Promise<boolean> => {
    try {
      setLoading(true);
      const tokenResponse = await authService.login({ username, password });
      authService.setToken(tokenResponse.access_token);

      // Get user information after successful login
      const userResponse = await authService.getCurrentUser();
      authService.setUser(userResponse);

      setUser(userResponse);
      setIsAuthenticated(true);
      return true;
    } catch (error) {
      console.error('Login failed:', error);
      return false;
    } finally {
      setLoading(false);
    }
  };

  const register = async (userData: UserCreate): Promise<boolean> => {
    try {
      setLoading(true);
      await authService.register(userData);
      
      // Automatically log in after successful registration
      return await login(userData.username, userData.password);
    } catch (error) {
      console.error('Registration failed:', error);
      return false;
    } finally {
      setLoading(false);
    }
  };

  const logout = () => {
    authService.removeToken();
    setUser(null);
    setIsAuthenticated(false);
  };

  const refreshToken = async (): Promise<boolean> => {
    try {
      const tokenResponse = await authService.refreshToken();
      authService.setToken(tokenResponse.access_token);
      return true;
    } catch (error) {
      console.error('Token refresh failed:', error);
      logout(); // Log out if refresh fails
      return false;
    }
  };

  return (
    <AuthContext.Provider value={{ 
      isAuthenticated, 
      user, 
      loading, 
      login, 
      register, 
      logout, 
      refreshToken 
    }}>
      {children}
    </AuthContext.Provider>
  );
}

export function useAuth() {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
}
