'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import userSecurityService from '@/services/userSecurityService';
import {
  SignonRequestDTO,
  SignonResponseDTO,
  InvalidKeyRequest,
  ExitRequest,
  FormErrors,
  SignonFormData
} from '@/types/userSecurity';

export default function SignonPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<SignonFormData>({
    username: '',
    password: '',
    rememberMe: false
  });
  const [errors, setErrors] = useState<FormErrors>({});
  const [isLoading, setIsLoading] = useState(false);
  const [generalError, setGeneralError] = useState<string>('');
  const [isInitialized, setIsInitialized] = useState(false);

  // Initialize page
  useEffect(() => {
    setIsInitialized(true);
    // Clear any existing authentication
    userSecurityService.clearAuth();
  }, []);

  // Clear errors when form data changes
  useEffect(() => {
    if (Object.keys(errors).length > 0) {
      setErrors({});
    }
    if (generalError) {
      setGeneralError('');
    }
  }, [formData.username, formData.password]);

  // Validate form fields
  const validateForm = useCallback((): boolean => {
    const newErrors: FormErrors = {};

    // Empty field validation
    if (!formData.username.trim()) {
      newErrors.username = 'User ID is required';
    }

    if (!formData.password.trim()) {
      newErrors.password = 'Password is required';
    }

    // Username format validation
    if (formData.username.trim() && formData.username.length < 3) {
      newErrors.username = 'User ID must be at least 3 characters';
    }

    // Password format validation
    if (formData.password.trim() && formData.password.length < 4) {
      newErrors.password = 'Password must be at least 4 characters';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  }, [formData]);

  // Handle form input changes
  const handleInputChange = useCallback((field: keyof SignonFormData, value: string | boolean) => {
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));
  }, []);

  // Handle authentication
  const handleAuthentication = useCallback(async (): Promise<void> => {
    if (!validateForm()) {
      return;
    }

    setIsLoading(true);
    setGeneralError('');

    try {
      const signonRequest: SignonRequestDTO = {
        username: formData.username.trim(),
        password: formData.password,
        rememberMe: formData.rememberMe
      };

      const response = await userSecurityService.signon(signonRequest);

      if (response.success && response.data) {
        const signonResponse: SignonResponseDTO = response.data;

        if (signonResponse.success && signonResponse.user) {
          // Check if password change is required
          if (signonResponse.requiresPasswordChange) {
            router.push('/change-password');
            return;
          }

          // Redirect based on user type/roles
          const userRoles = signonResponse.user.roles || [];
          
          if (userRoles.includes('ADMIN')) {
            router.push('/admin');
          } else if (userRoles.includes('MANAGER')) {
            router.push('/manager');
          } else {
            router.push('/general');
          }
        } else {
          // Handle authentication failure
          const errorMessage = signonResponse.message || 'Authentication failed';
          
          if (errorMessage.toLowerCase().includes('user not found')) {
            setErrors({ username: 'User ID not found' });
          } else if (errorMessage.toLowerCase().includes('password') || 
                     errorMessage.toLowerCase().includes('invalid credentials')) {
            setErrors({ password: 'Invalid password' });
          } else if (errorMessage.toLowerCase().includes('locked') || 
                     errorMessage.toLowerCase().includes('disabled')) {
            setGeneralError('Account is locked or disabled. Contact administrator.');
          } else {
            setGeneralError(errorMessage);
          }
        }
      } else {
        // Handle service error
        const errorMessage = response.error || 'Authentication service unavailable';
        
        if (errorMessage.toLowerCase().includes('network') || 
            errorMessage.toLowerCase().includes('connection')) {
          setGeneralError('Network connection error. Please try again.');
        } else if (errorMessage.toLowerCase().includes('timeout')) {
          setGeneralError('Request timeout. Please try again.');
        } else {
          setGeneralError(errorMessage);
        }
      }
    } catch (error) {
      console.error('Authentication error:', error);
      setGeneralError('An unexpected error occurred. Please try again.');
    } finally {
      setIsLoading(false);
    }
  }, [formData, validateForm, router]);

  // Handle PF3 Exit
  const handleExit = useCallback(async (): Promise<void> => {
    setIsLoading(true);

    try {
      const exitRequest: ExitRequest = {
        userId: formData.username || undefined
      };

      await userSecurityService.exit(exitRequest);
      
      // Clear form and redirect to home or close application
      setFormData({ username: '', password: '', rememberMe: false });
      setErrors({});
      setGeneralError('');
      
      // In a real application, this might close the window or redirect to a goodbye page
      router.push('/');
    } catch (error) {
      console.error('Exit error:', error);
      // Even if exit fails, clear the form and redirect
      router.push('/');
    } finally {
      setIsLoading(false);
    }
  }, [formData.username, router]);

  // Handle invalid key press
  const handleInvalidKey = useCallback(async (key: string): Promise<void> => {
    try {
      const invalidKeyRequest: InvalidKeyRequest = {
        username: formData.username || undefined,
        attemptedKey: key,
        timestamp: new Date().toISOString()
      };

      const response = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.success && response.data) {
        const { message, lockoutDuration, remainingAttempts } = response.data;
        
        if (lockoutDuration && lockoutDuration > 0) {
          setGeneralError(`Account temporarily locked for ${lockoutDuration} minutes due to invalid key attempts.`);
        } else if (remainingAttempts !== undefined) {
          setGeneralError(`Invalid key pressed. ${remainingAttempts} attempts remaining.`);
        } else if (message) {
          setGeneralError(message);
        } else {
          setGeneralError('Invalid key pressed. Please use ENTER to sign on or F3 to exit.');
        }
      } else {
        setGeneralError('Invalid key pressed. Please use ENTER to sign on or F3 to exit.');
      }
    } catch (error) {
      console.error('Invalid key handling error:', error);
      setGeneralError('Invalid key pressed. Please use ENTER to sign on or F3 to exit.');
    }
  }, [formData.username]);

  // Handle keyboard events
  const handleKeyDown = useCallback(async (event: React.KeyboardEvent): Promise<void> => {
    // Prevent default for function keys
    if (event.key.startsWith('F') || event.key === 'Enter') {
      event.preventDefault();
    }

    if (isLoading) {
      return;
    }

    switch (event.key) {
      case 'Enter':
        await handleAuthentication();
        break;
      
      case 'F3':
        await handleExit();
        break;
      
      case 'F1':
      case 'F2':
      case 'F4':
      case 'F5':
      case 'F6':
      case 'F7':
      case 'F8':
      case 'F9':
      case 'F10':
      case 'F11':
      case 'F12':
        await handleInvalidKey(event.key);
        break;
      
      default:
        // Allow normal typing in input fields
        if (!event.ctrlKey && !event.altKey && !event.metaKey) {
          return;
        }
        
        // Handle other special key combinations as invalid
        if (event.ctrlKey || event.altKey || event.metaKey) {
          await handleInvalidKey(`${event.ctrlKey ? 'Ctrl+' : ''}${event.altKey ? 'Alt+' : ''}${event.metaKey ? 'Meta+' : ''}${event.key}`);
        }
        break;
    }
  }, [isLoading, handleAuthentication, handleExit, handleInvalidKey]);

  // Handle form submission
  const handleSubmit = useCallback(async (event: React.FormEvent): Promise<void> => {
    event.preventDefault();
    await handleAuthentication();
  }, [handleAuthentication]);

  // Don't render until initialized
  if (!isInitialized) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-2 text-gray-600">Initializing...</p>
        </div>
      </div>
    );
  }

  return (
    <div 
      className="min-h-screen flex items-center justify-center bg-gray-50 py-12 px-4 sm:px-6 lg:px-8"
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-md w-full space-y-8">
        <div>
          <div className="mx-auto h-12 w-12 flex items-center justify-center rounded-full bg-blue-100">
            <svg
              className="h-6 w-6 text-blue-600"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"
              />
            </svg>
          </div>
          <h2 className="mt-6 text-center text-3xl font-extrabold text-gray-900">
            Sign On
          </h2>
          <p className="mt-2 text-center text-sm text-gray-600">
            Enter your credentials to access the system
          </p>
        </div>

        <form className="mt-8 space-y-6" onSubmit={handleSubmit}>
          <div className="space-y-4">
            <div>
              <label htmlFor="username" className="block text-sm font-medium text-gray-700">
                User ID
              </label>
              <Input
                id="username"
                name="username"
                type="text"
                autoComplete="username"
                required
                value={formData.username}
                onChange={(e) => handleInputChange('username', e.target.value)}
                placeholder="Enter your User ID"
                disabled={isLoading}
                className={`mt-1 ${errors.username ? 'border-red-500 focus:border-red-500 focus:ring-red-500' : ''}`}
                autoFocus
              />
              {errors.username && (
                <p className="mt-1 text-sm text-red-600">{errors.username}</p>
              )}
            </div>

            <div>
              <label htmlFor="password" className="block text-sm font-medium text-gray-700">
                Password
              </label>
              <Input
                id="password"
                name="password"
                type="password"
                autoComplete="current-password"
                required
                value={formData.password}
                onChange={(e) => handleInputChange('password', e.target.value)}
                placeholder="Enter your Password"
                disabled={isLoading}
                className={`mt-1 ${errors.password ? 'border-red-500 focus:border-red-500 focus:ring-red-500' : ''}`}
              />
              {errors.password && (
                <p className="mt-1 text-sm text-red-600">{errors.password}</p>
              )}
            </div>

            <div className="flex items-center">
              <input
                id="rememberMe"
                name="rememberMe"
                type="checkbox"
                checked={formData.rememberMe}
                onChange={(e) => handleInputChange('rememberMe', e.target.checked)}
                disabled={isLoading}
                className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
              />
              <label htmlFor="rememberMe" className="ml-2 block text-sm text-gray-900">
                Remember me
              </label>
            </div>
          </div>

          {generalError && (
            <div className="rounded-md bg-red-50 p-4">
              <div className="flex">
                <div className="flex-shrink-0">
                  <svg
                    className="h-5 w-5 text-red-400"
                    xmlns="http://www.w3.org/2000/svg"
                    viewBox="0 0 20 20"
                    fill="currentColor"
                  >
                    <path
                      fillRule="evenodd"
                      d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
                      clipRule="evenodd"
                    />
                  </svg>
                </div>
                <div className="ml-3">
                  <p className="text-sm text-red-800">{generalError}</p>
                </div>
              </div>
            </div>
          )}

          <div className="space-y-3">
            <Button
              type="submit"
              disabled={isLoading}
              className="group relative w-full flex justify-center py-2 px-4 border border-transparent text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {isLoading ? (
                <>
                  <svg
                    className="animate-spin -ml-1 mr-3 h-5 w-5 text-white"
                    xmlns="http://www.w3.org/2000/svg"
                    fill="none"
                    viewBox="0 0 24 24"
                  >
                    <circle
                      className="opacity-25"
                      cx="12"
                      cy="12"
                      r="10"
                      stroke="currentColor"
                      strokeWidth="4"
                    ></circle>
                    <path
                      className="opacity-75"
                      fill="currentColor"
                      d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                    ></path>
                  </svg>
                  Signing On...
                </>
              ) : (
                'Sign On (ENTER)'
              )}
            </Button>

            <Button
              type="button"
              onClick={handleExit}
              disabled={isLoading}
              variant="secondary"
              className="w-full flex justify-center py-2 px-4 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Exit (F3)
            </Button>
          </div>

          <div className="text-center">
            <p className="text-xs text-gray-500">
              Press ENTER to sign on or F3 to exit
            </p>
          </div>
        </form>
      </div>
    </div>
  );
}