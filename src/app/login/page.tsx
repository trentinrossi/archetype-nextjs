'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { SignonRequest, SignonResponse, APIResponse, UserRole } from '@/types/user';
import { userService } from '@/services/userService';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';

interface LoginState {
  userId: string;
  password: string;
  loading: boolean;
  error: string;
  successMessage: string;
  showThankYou: boolean;
}

export default function LoginPage() {
  const [state, setState] = useState<LoginState>({
    userId: '',
    password: '',
    loading: false,
    error: '',
    successMessage: '',
    showThankYou: false,
  });

  const updateState = useCallback((updates: Partial<LoginState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const clearMessages = useCallback(() => {
    updateState({ error: '', successMessage: '' });
  }, [updateState]);

  const handleUserIdChange = useCallback((value: string) => {
    clearMessages();
    updateState({ userId: value.toUpperCase().slice(0, 8) });
  }, [clearMessages, updateState]);

  const handlePasswordChange = useCallback((value: string) => {
    clearMessages();
    updateState({ password: value.slice(0, 8) });
  }, [clearMessages, updateState]);

  const validateFields = useCallback((): boolean => {
    if (!state.userId.trim()) {
      updateState({ error: 'User ID is required' });
      return false;
    }

    if (state.userId.length !== 8) {
      updateState({ error: 'User ID must be exactly 8 characters' });
      return false;
    }

    if (!state.password) {
      updateState({ error: 'Password is required' });
      return false;
    }

    if (state.password.length !== 8) {
      updateState({ error: 'Password must be exactly 8 characters' });
      return false;
    }

    return true;
  }, [state.userId, state.password, updateState]);

  const handleLogin = useCallback(async () => {
    if (!validateFields()) {
      return;
    }

    clearMessages();
    updateState({ loading: true });

    try {
      const signonRequest: SignonRequest = {
        userId: state.userId,
        password: state.password,
      };

      const response: APIResponse<SignonResponse> = await userService.signon(signonRequest);

      if (response.success && response.data?.success && response.data.user) {
        const user = response.data.user;
        updateState({ 
          successMessage: `Welcome, ${user.firstName} ${user.lastName}!`,
          loading: false 
        });

        // Redirect based on user type after short delay
        setTimeout(() => {
          if (user.userType === 'A') {
            // Admin user - redirect to COADM01C (Admin screen)
            window.location.href = '/admin';
          } else {
            // General user - redirect to COMEN01C (General user screen)
            window.location.href = '/users';
          }
        }, 1500);
      } else {
        // Handle authentication errors
        let errorMessage = 'Authentication failed';
        
        if (response.error?.statusCode === 401) {
          errorMessage = 'Invalid User ID or Password';
        } else if (response.error?.statusCode === 404) {
          errorMessage = 'User not found';
        } else if (response.error?.statusCode === 403) {
          errorMessage = 'Access denied';
        } else if (response.error?.message) {
          errorMessage = response.error.message;
        }

        updateState({ 
          error: errorMessage,
          loading: false 
        });
      }
    } catch (error) {
      updateState({ 
        error: 'System error occurred. Please try again later.',
        loading: false 
      });
      console.error('Login error:', error);
    }
  }, [validateFields, clearMessages, updateState, state.userId, state.password]);

  const handleClear = useCallback(() => {
    setState({
      userId: '',
      password: '',
      loading: false,
      error: '',
      successMessage: '',
      showThankYou: false,
    });
  }, []);

  const handleExit = useCallback(() => {
    updateState({ showThankYou: true });
    
    setTimeout(() => {
      if (typeof window !== 'undefined') {
        window.location.href = '/';
      }
    }, 2000);
  }, [updateState]);

  const handleInvalidKeyPress = useCallback(() => {
    updateState({ error: 'Invalid key pressed. Use ENTER to login, PF3 to exit.' });
  }, [updateState]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      // Prevent default for function keys
      if (event.key.startsWith('F') || event.key === 'Enter') {
        event.preventDefault();
      }

      switch (event.key) {
        case 'Enter':
          if (!state.loading && !state.showThankYou) {
            handleLogin();
          }
          break;
        case 'F3':
          if (!state.loading) {
            handleExit();
          }
          break;
        case 'F4':
          if (!state.loading && !state.showThankYou) {
            handleClear();
          }
          break;
        case 'F1':
        case 'F2':
        case 'F5':
        case 'F6':
        case 'F7':
        case 'F8':
        case 'F9':
        case 'F10':
        case 'F11':
        case 'F12':
          handleInvalidKeyPress();
          break;
        default:
          // Allow normal typing
          break;
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [state.loading, state.showThankYou, handleLogin, handleExit, handleClear, handleInvalidKeyPress]);

  // Auto-focus on User ID field when component mounts
  useEffect(() => {
    const userIdInput = document.getElementById('userId');
    if (userIdInput) {
      userIdInput.focus();
    }
  }, []);

  if (state.showThankYou) {
    return (
      <div className="min-h-screen bg-muted/20 flex items-center justify-center px-4">
        <div className="bg-background border border-border rounded-lg p-8 max-w-md mx-auto text-center">
          <div className="mb-6">
            <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4">
              <span className="text-2xl text-green-600">✓</span>
            </div>
            <h1 className="text-2xl font-bold text-foreground mb-2">Thank You</h1>
            <p className="text-muted-foreground">
              Thank you for using the system. You will be redirected shortly.
            </p>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-muted/20 flex items-center justify-center px-4">
      <div className="bg-background border border-border rounded-lg p-8 max-w-md mx-auto w-full">
        {/* Header */}
        <div className="mb-8 text-center">
          <h1 className="text-3xl font-bold text-foreground mb-2">System Login</h1>
          <p className="text-muted-foreground mb-4">
            Enter your credentials to access the system
          </p>
          <div className="text-sm text-muted-foreground">
            <p>User ID: 8 characters | Password: 8 characters</p>
          </div>
        </div>

        {/* Success Message */}
        {state.successMessage && (
          <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-md">
            <p className="text-green-800 text-sm font-medium">{state.successMessage}</p>
            <p className="text-green-700 text-xs mt-1">Redirecting...</p>
          </div>
        )}

        {/* Error Message */}
        {state.error && (
          <div className="mb-6 p-4 bg-destructive/10 border border-destructive/20 rounded-md">
            <p className="text-destructive text-sm">{state.error}</p>
          </div>
        )}

        {/* Login Form */}
        <div className="space-y-6">
          {/* User ID Field */}
          <div>
            <Input
              id="userId"
              label="User ID *"
              value={state.userId}
              onChange={(e) => handleUserIdChange(e.target.value)}
              maxLength={8}
              placeholder="Enter 8-character User ID"
              className="font-mono text-center"
              disabled={state.loading}
              autoComplete="username"
            />
            <p className="text-xs text-muted-foreground mt-1 text-center">
              {state.userId.length}/8 characters
            </p>
          </div>

          {/* Password Field */}
          <div>
            <Input
              id="password"
              label="Password *"
              type="password"
              value={state.password}
              onChange={(e) => handlePasswordChange(e.target.value)}
              maxLength={8}
              placeholder="Enter 8-character Password"
              className="font-mono text-center"
              disabled={state.loading}
              autoComplete="current-password"
            />
            <p className="text-xs text-muted-foreground mt-1 text-center">
              {state.password.length}/8 characters
            </p>
          </div>

          {/* Action Buttons */}
          <div className="space-y-4">
            <Button
              onClick={handleLogin}
              disabled={state.loading || !state.userId.trim() || !state.password}
              variant="primary"
              className="w-full"
            >
              {state.loading ? 'Signing In...' : 'Sign In (ENTER)'}
            </Button>

            <div className="flex gap-2">
              <Button
                onClick={handleClear}
                disabled={state.loading}
                variant="outline"
                className="flex-1"
              >
                Clear (F4)
              </Button>
              <Button
                onClick={handleExit}
                disabled={state.loading}
                variant="outline"
                className="flex-1"
              >
                Exit (F3)
              </Button>
            </div>
          </div>
        </div>

        {/* System Information */}
        <div className="mt-8 pt-6 border-t border-border">
          <div className="text-center">
            <p className="text-xs text-muted-foreground mb-2">
              <strong>Security Notice:</strong> This is a secure system. 
              Unauthorized access is prohibited.
            </p>
            <p className="text-xs text-muted-foreground mb-4">
              All login attempts are logged and monitored.
            </p>
          </div>

          {/* User Type Information */}
          <div className="bg-blue-50 border border-blue-200 rounded-md p-3">
            <p className="text-xs text-blue-800 font-medium mb-1">User Access Levels:</p>
            <div className="text-xs text-blue-700 space-y-1">
              <p>• <strong>Admin (A):</strong> Full system administration access</p>
              <p>• <strong>User (U):</strong> General user operations access</p>
            </div>
          </div>
        </div>

        {/* Keyboard Shortcuts Help */}
        <div className="mt-6 pt-4 border-t border-border">
          <p className="text-xs text-muted-foreground text-center">
            <strong>Keyboard Shortcuts:</strong> ENTER = Sign In | F3 = Exit | F4 = Clear Fields
          </p>
        </div>

        {/* Loading Indicator */}
        {state.loading && (
          <div className="mt-4 flex items-center justify-center">
            <div className="animate-spin rounded-full h-6 w-6 border-b-2 border-primary"></div>
            <span className="ml-2 text-sm text-muted-foreground">Authenticating...</span>
          </div>
        )}
      </div>
    </div>
  );
}