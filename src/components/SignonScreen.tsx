'use client';

import React, { useState, useEffect, useRef } from 'react';
import { useRouter } from 'next/navigation';
import { userSecurityService } from '@/services/userSecurityService';
import { SignonRequestDTO, SignonResponseDTO, ERROR_CODES } from '@/types/userSecurity';

interface SignonScreenProps {
  onSignonSuccess?: (response: SignonResponseDTO) => void;
  onExit?: () => void;
  className?: string;
}

const SignonScreen: React.FC<SignonScreenProps> = ({
  onSignonSuccess,
  onExit,
  className = ''
}) => {
  const router = useRouter();
  const [userId, setUserId] = useState<string>('');
  const [password, setPassword] = useState<string>('');
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [errorMessage, setErrorMessage] = useState<string>('');
  const [userIdError, setUserIdError] = useState<string>('');
  const [passwordError, setPasswordError] = useState<string>('');
  const [rememberMe, setRememberMe] = useState<boolean>(false);

  const userIdRef = useRef<HTMLInputElement>(null);
  const passwordRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    // Focus on User ID field when component mounts
    if (userIdRef.current) {
      userIdRef.current.focus();
    }
  }, []);

  const clearErrors = () => {
    setErrorMessage('');
    setUserIdError('');
    setPasswordError('');
  };

  const validateFields = (): boolean => {
    let isValid = true;
    clearErrors();

    if (!userId.trim()) {
      setUserIdError('Please enter User ID');
      isValid = false;
    } else if (userId.length > 8) {
      setUserIdError('User ID cannot exceed 8 characters');
      isValid = false;
    }

    if (!password.trim()) {
      setPasswordError('Please enter Password');
      isValid = false;
    } else if (password.length > 8) {
      setPasswordError('Password cannot exceed 8 characters');
      isValid = false;
    }

    return isValid;
  };

  const handleSignon = async () => {
    if (!validateFields()) {
      return;
    }

    setIsLoading(true);
    clearErrors();

    try {
      const signonRequest: SignonRequestDTO = {
        userId: userId.trim(),
        password: password.trim(),
        systemId: 'COSGN00C',
        workstationId: typeof window !== 'undefined' ? window.location.hostname : undefined
      };

      const response = await userSecurityService.signon(signonRequest, rememberMe);

      if (response.success && response.data) {
        const signonData = response.data;
        
        if (signonData.success) {
          // Clear form
          setUserId('');
          setPassword('');
          
          // Call success callback if provided
          if (onSignonSuccess) {
            onSignonSuccess(signonData);
          } else {
            // Default redirection based on user type
            if (signonData.userType === 'ADMIN') {
              router.push('/admin/dashboard');
            } else {
              router.push('/dashboard');
            }
          }
        } else {
          // Handle authentication failure
          handleSignonError(signonData.errorCode, signonData.errorMessage, signonData.remainingAttempts);
        }
      } else {
        // Handle API error
        setErrorMessage(response.error?.message || 'Authentication failed. Please try again.');
      }
    } catch (error) {
      console.error('Signon error:', error);
      setErrorMessage('System error occurred. Please try again later.');
    } finally {
      setIsLoading(false);
    }
  };

  const handleSignonError = (errorCode?: string, errorMessage?: string, remainingAttempts?: number) => {
    switch (errorCode) {
      case ERROR_CODES.INVALID_CREDENTIALS:
        setErrorMessage('Wrong Password');
        break;
      
      case ERROR_CODES.USER_NOT_FOUND:
        setErrorMessage('User not found');
        break;
      
      case ERROR_CODES.USER_INACTIVE:
        setErrorMessage('User account is inactive');
        break;
      
      case ERROR_CODES.USER_LOCKED:
        setErrorMessage('User account is locked');
        break;
      
      case ERROR_CODES.PASSWORD_EXPIRED:
        setErrorMessage('Password has expired');
        break;
      
      case ERROR_CODES.SYSTEM_ERROR:
        setErrorMessage('System error occurred');
        break;
      
      default:
        setErrorMessage(errorMessage || 'Authentication failed');
        break;
    }
  };

  const handleExit = async () => {
    setIsLoading(true);
    
    try {
      await userSecurityService.exit();
      
      if (onExit) {
        onExit();
      } else {
        // Show thank you message
        setErrorMessage('Thank you for using CardDemo application');
        setTimeout(() => {
          if (typeof window !== 'undefined') {
            window.close();
          }
        }, 2000);
      }
    } catch (error) {
      console.error('Exit error:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const handleInvalidKey = async () => {
    try {
      await userSecurityService.invalidateKey();
      setErrorMessage('Invalid key pressed');
    } catch (error) {
      console.error('Invalid key error:', error);
    }
  };

  const handleKeyDown = (event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!isLoading) {
          handleSignon();
        }
        break;
      
      case 'F3':
        event.preventDefault();
        if (!isLoading) {
          handleExit();
        }
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
        event.preventDefault();
        handleInvalidKey();
        break;
      
      default:
        // Allow normal key processing
        break;
    }
  };

  const handleUserIdChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    if (value.length <= 8) {
      setUserId(value);
      if (userIdError) {
        setUserIdError('');
      }
    }
  };

  const handlePasswordChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    if (value.length <= 8) {
      setPassword(value);
      if (passwordError) {
        setPasswordError('');
      }
    }
  };

  const handleUserIdKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter' && userId.trim() && passwordRef.current) {
      e.preventDefault();
      passwordRef.current.focus();
    } else {
      handleKeyDown(e);
    }
  };

  return (
    <div 
      className={`min-h-screen bg-gray-50 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8 ${className}`}
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-md w-full space-y-8">
        <div>
          <h2 className="mt-6 text-center text-3xl font-extrabold text-gray-900">
            CardDemo
          </h2>
          <p className="mt-2 text-center text-sm text-gray-600">
            System Sign On
          </p>
          <p className="text-center text-xs text-gray-500">
            COSGN00C - {new Date().toLocaleDateString()} {new Date().toLocaleTimeString()}
          </p>
        </div>
        
        <form className="mt-8 space-y-6" onSubmit={(e) => e.preventDefault()}>
          <div className="space-y-4">
            {/* User ID Field */}
            <div>
              <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-1">
                User ID
              </label>
              <input
                ref={userIdRef}
                id="userId"
                name="userId"
                type="text"
                value={userId}
                onChange={handleUserIdChange}
                onKeyDown={handleUserIdKeyDown}
                placeholder="Enter User ID (max 8 chars)"
                maxLength={8}
                className={`appearance-none rounded-md relative block w-full px-3 py-2 border ${
                  userIdError ? 'border-red-500' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 focus:z-10 sm:text-sm`}
                disabled={isLoading}
                autoComplete="username"
              />
              {userIdError && (
                <p className="mt-1 text-sm text-red-600">{userIdError}</p>
              )}
            </div>

            {/* Password Field */}
            <div>
              <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-1">
                Password
              </label>
              <input
                ref={passwordRef}
                id="password"
                name="password"
                type="password"
                value={password}
                onChange={handlePasswordChange}
                onKeyDown={handleKeyDown}
                placeholder="Enter Password (max 8 chars)"
                maxLength={8}
                className={`appearance-none rounded-md relative block w-full px-3 py-2 border ${
                  passwordError ? 'border-red-500' : 'border-gray-300'
                } placeholder-gray-500 text-gray-900 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 focus:z-10 sm:text-sm`}
                disabled={isLoading}
                autoComplete="current-password"
              />
              {passwordError && (
                <p className="mt-1 text-sm text-red-600">{passwordError}</p>
              )}
            </div>
          </div>

          {/* Error Message */}
          {errorMessage && (
            <div className={`p-3 rounded-md ${
              errorMessage.includes('Thank you') 
                ? 'bg-green-50 border border-green-200' 
                : 'bg-red-50 border border-red-200'
            }`}>
              <p className={`text-sm ${
                errorMessage.includes('Thank you') ? 'text-green-800' : 'text-red-800'
              }`}>
                {errorMessage}
              </p>
            </div>
          )}

          {/* Action Buttons */}
          <div className="flex space-x-4">
            <button
              type="button"
              onClick={handleSignon}
              disabled={isLoading || !userId.trim() || !password.trim()}
              className="group relative w-full flex justify-center py-2 px-4 border border-transparent text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {isLoading ? 'Signing On...' : 'Sign On (Enter)'}
            </button>
            
            <button
              type="button"
              onClick={handleExit}
              disabled={isLoading}
              className="group relative w-full flex justify-center py-2 px-4 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Exit (F3)
            </button>
          </div>

          {/* Function Key Instructions */}
          <div className="text-center">
            <p className="text-xs text-gray-500">
              Press Enter to sign on • Press F3 to exit
            </p>
          </div>
        </form>
      </div>
    </div>
  );
};

export default SignonScreen;