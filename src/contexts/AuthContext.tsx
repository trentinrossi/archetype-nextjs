'use client';

import React, { createContext, useContext, useState, useEffect } from 'react';
import { authService } from '@/services/authService';
import { AuthUser } from '@/types/auth';

interface AuthContextType {
  isAuthenticated: boolean;
  user: AuthUser | null;
  loading: boolean;
  login: (userId: string, password: string) => Promise<boolean>;
  logout: () => void;
  isAdmin: () => boolean;
  isRegularUser: () => boolean;
}

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [user, setUser] = useState<AuthUser | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const initializeAuth = async () => {
      const token = authService.getAuthToken();
      const savedUserData = authService.getAuthUser();

      if (token && savedUserData) {
        setUser(savedUserData);
        setIsAuthenticated(true);
      }
      setLoading(false);
    };

    initializeAuth();
  }, []);

  const login = async (userId: string, password: string): Promise<boolean> => {
    try {
      setLoading(true);
      const response = await authService.login(userId, password);
      
      const authUser: AuthUser = {
        userId: response.userId,
        firstName: response.firstName,
        lastName: response.lastName,
        userType: response.userType,
      };

      setUser(authUser);
      setIsAuthenticated(true);
      return true;
    } catch (error) {
      console.error('Login failed:', error);
      return false;
    } finally {
      setLoading(false);
    }
  };

  const logout = () => {
    authService.logout();
    setUser(null);
    setIsAuthenticated(false);
  };

  const isAdmin = (): boolean => {
    return authService.isAdmin();
  };

  const isRegularUser = (): boolean => {
    return authService.isRegularUser();
  };

  return (
    <AuthContext.Provider value={{ 
      isAuthenticated, 
      user, 
      loading, 
      login, 
      logout,
      isAdmin,
      isRegularUser
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
