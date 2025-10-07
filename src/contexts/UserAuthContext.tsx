'use client';

import React, { createContext, useContext, useState, useEffect } from 'react';
import { userService } from '@/services/userService';
import { 
  UserSecurityDTO, 
  SignonRequestDTO, 
  SignonResponseDTO, 
  AuthContextType 
} from '@/types/user';

const UserAuthContext = createContext<AuthContextType | undefined>(undefined);

export function UserAuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<UserSecurityDTO | null>(null);
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const initializeAuth = async () => {
      try {
        // Check if user has valid session
        const isValid = await userService.validateSession();
        
        if (isValid) {
          const currentUser = await userService.getCurrentUser();
          if (currentUser) {
            setUser(currentUser);
            setIsAuthenticated(true);
          }
        }
      } catch (error) {
        console.error('Auth initialization error:', error);
        setUser(null);
        setIsAuthenticated(false);
      } finally {
        setIsLoading(false);
      }
    };

    initializeAuth();
  }, []);

  const login = async (credentials: SignonRequestDTO): Promise<SignonResponseDTO> => {
    try {
      setIsLoading(true);
      const response = await userService.signon(credentials);
      
      if (response.success && response.user) {
        setUser(response.user);
        setIsAuthenticated(true);
      }
      
      return response;
    } catch (error) {
      console.error('Login error:', error);
      const errorMessage = error instanceof Error ? error.message : 'Login failed';
      return {
        success: false,
        message: errorMessage,
        errorCode: 'LOGIN_ERROR',
      };
    } finally {
      setIsLoading(false);
    }
  };

  const logout = async (): Promise<void> => {
    try {
      await userService.exit();
    } catch (error) {
      console.error('Logout error:', error);
    } finally {
      setUser(null);
      setIsAuthenticated(false);
    }
  };

  const validateSession = async (): Promise<boolean> => {
    try {
      const isValid = await userService.validateSession();
      
      if (!isValid) {
        setUser(null);
        setIsAuthenticated(false);
      }
      
      return isValid;
    } catch (error) {
      console.error('Session validation error:', error);
      setUser(null);
      setIsAuthenticated(false);
      return false;
    }
  };

  return (
    <UserAuthContext.Provider value={{
      user,
      isAuthenticated,
      isLoading,
      login,
      logout,
      validateSession,
    }}>
      {children}
    </UserAuthContext.Provider>
  );
}

export function useUserAuth() {
  const context = useContext(UserAuthContext);
  if (context === undefined) {
    throw new Error('useUserAuth must be used within a UserAuthProvider');
  }
  return context;
}