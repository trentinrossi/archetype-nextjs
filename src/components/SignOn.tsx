'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui/Button';
import { SignonRequestDTO, SignonResponseDTO } from '@/types/user';
import { userService } from '@/services/userService';
import { useUserAuth } from '@/contexts/UserAuthContext';

interface SignOnProps {
  onSignonSuccess?: (response: SignonResponseDTO) => void;
  onExit?: () => void;
  className?: string;
}

interface SignOnState {
  userId: string;
  password: string;
  isLoading: boolean;
  error: string;
  showErrorModal: boolean;
  isSubmitting: boolean;
}

const SignOn: React.FC<SignOnProps> = ({
  onSignonSuccess,
  onExit,
  className = '',
}) => {
  const router = useRouter();
  const { login } = useUserAuth();
  const [state, setState] = useState<SignOnState>({
    userId: '',
    password: '',
    isLoading: false,
    error: '',
    showErrorModal: false,
    isSubmitting: false,
  });

  const updateState = useCallback((updates: Partial<SignOnState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const validateInput = useCallback((credentials: SignonRequestDTO): string[] => {
    return userService.validateCredentials(credentials);
  }, []);

  const handleSignOn = useCallback(async () => {
    if (state.isSubmitting) return;

    const credentials: SignonRequestDTO = {
      userId: userService.sanitizeUserId(state.userId),
      password: state.password,
    };

    const validationErrors = validateInput(credentials);
    if (validationErrors.length > 0) {
      updateState({
        error: validationErrors.join('. '),
        showErrorModal: true,
      });
      return;
    }

    updateState({ isSubmitting: true, error: '' });

    try {
      const response = await login(credentials);
      
      if (response.success && response.user) {
        if (onSignonSuccess) {
          onSignonSuccess(response);
        } else {
          // Default redirect to user list
          router.push('/users');
        }
      } else {
        updateState({
          error: response.message || 'Authentication failed',
          showErrorModal: true,
          isSubmitting: false,
        });
      }
    } catch (error: any) {
      let errorMessage = error.message || 'Authentication failed';

      // Handle specific error cases
      if (error.message?.includes('401')) {
        errorMessage = 'Wrong Password';
      } else if (error.message?.includes('403')) {
        errorMessage = 'User account is inactive';
      } else if (error.message?.includes('400')) {
        errorMessage = 'Invalid input data';
      } else if (error.message?.includes('500')) {
        errorMessage = 'System error occurred';
      }

      updateState({
        error: errorMessage,
        showErrorModal: true,
        isSubmitting: false,
      });
    }
  }, [state.userId, state.password, state.isSubmitting, onSignonSuccess, router, login, updateState, validateInput]);

  const handleExit = useCallback(async () => {
    if (state.isSubmitting) return;

    updateState({ isSubmitting: true });

    try {
      await userService.exit();
      if (onExit) {
        onExit();
      } else {
        // Show thank you message and close
        alert('Thank you for using CardDemo Application');
        window.close();
      }
    } catch (error: any) {
      console.error('Exit error:', error);
      if (onExit) {
        onExit();
      } else {
        alert('Thank you for using CardDemo Application');
        window.close();
      }
    } finally {
      updateState({ isSubmitting: false });
    }
  }, [state.isSubmitting, onExit, updateState]);

  const handleInvalidKey = useCallback(async (key: string) => {
    try {
      await userService.handleInvalidKey(key);
      updateState({
        error: 'Invalid key pressed',
        showErrorModal: true,
      });
    } catch (error: any) {
      console.error('Invalid key handling error:', error);
      updateState({
        error: 'Invalid key pressed',
        showErrorModal: true,
      });
    }
  }, [updateState]);

  const handleKeyDown = useCallback(async (event: KeyboardEvent) => {
    if (state.isSubmitting) return;

    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        await handleSignOn();
        break;
      case 'F3':
        event.preventDefault();
        await handleExit();
        break;
      case 'Escape':
        if (state.showErrorModal) {
          updateState({ showErrorModal: false, error: '' });
        }
        break;
      default:
        // Handle other function keys as invalid
        if (event.key.startsWith('F') && event.key !== 'F3') {
          event.preventDefault();
          await handleInvalidKey(event.key);
        }
        break;
    }
  }, [state.isSubmitting, state.showErrorModal, handleSignOn, handleExit, handleInvalidKey, updateState]);

  const handleInputChange = useCallback((field: 'userId' | 'password') => (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    const value = event.target.value;
    
    if (field === 'userId') {
      // Auto-uppercase and limit to 8 characters
      const sanitized = value.toUpperCase().slice(0, 8);
      updateState({ userId: sanitized });
    } else {
      // Limit password to 8 characters
      const limited = value.slice(0, 8);
      updateState({ password: limited });
    }
  }, [updateState]);

  const closeErrorModal = useCallback(() => {
    updateState({ showErrorModal: false, error: '' });
  }, [updateState]);

  const handleFormSubmit = useCallback((event: React.FormEvent) => {
    event.preventDefault();
    handleSignOn();
  }, [handleSignOn]);

  useEffect(() => {
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyDown]);

  useEffect(() => {
    // Focus on User ID field when component mounts
    const userIdInput = document.getElementById('userId');
    if (userIdInput) {
      userIdInput.focus();
    }
  }, []);

  return (
    <div className={`min-h-screen bg-blue-900 flex items-center justify-center p-4 ${className}`}>
      <div className="bg-white rounded-lg shadow-2xl p-8 w-full max-w-md">
        {/* Application Title */}
        <div className="text-center mb-8">
          <h1 className="text-3xl font-bold text-blue-900 mb-2">
            CardDemo
          </h1>
          <h2 className="text-xl text-gray-600">
            Sign On - COSGN00C
          </h2>
          <div className="text-sm text-gray-500 mt-2">
            <p>Program: COSGN00C | Transaction: CC00</p>
            <p>{new Date().toLocaleDateString()} | {new Date().toLocaleTimeString()}</p>
          </div>
        </div>

        {/* Sign On Form */}
        <form onSubmit={handleFormSubmit} className="space-y-6">
          <div>
            <label 
              htmlFor="userId" 
              className="block text-sm font-medium text-gray-700 mb-2"
            >
              User ID:
            </label>
            <input
              id="userId"
              type="text"
              value={state.userId}
              onChange={handleInputChange('userId')}
              placeholder="Enter User ID"
              maxLength={8}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 uppercase"
              disabled={state.isSubmitting}
              autoComplete="username"
              required
            />
          </div>

          <div>
            <label 
              htmlFor="password" 
              className="block text-sm font-medium text-gray-700 mb-2"
            >
              Password:
            </label>
            <input
              id="password"
              type="password"
              value={state.password}
              onChange={handleInputChange('password')}
              placeholder="Enter Password"
              maxLength={8}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={state.isSubmitting}
              autoComplete="current-password"
              required
            />
          </div>

          {/* Error Message */}
          {state.error && (
            <div className="bg-red-50 border border-red-200 rounded-md p-3">
              <div className="flex">
                <div className="flex-shrink-0">
                  <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                  </svg>
                </div>
                <div className="ml-3">
                  <p className="text-sm text-red-800">{state.error}</p>
                </div>
                <div className="ml-auto pl-3">
                  <button
                    type="button"
                    onClick={closeErrorModal}
                    className="inline-flex text-red-400 hover:text-red-600"
                  >
                    <span className="sr-only">Dismiss</span>
                    <svg className="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clipRule="evenodd" />
                    </svg>
                  </button>
                </div>
              </div>
            </div>
          )}

          {/* Action Buttons */}
          <div className="flex space-x-4 pt-4">
            <Button
              type="submit"
              variant="primary"
              className="flex-1"
              disabled={state.isSubmitting || !state.userId.trim() || !state.password.trim()}
            >
              {state.isSubmitting ? 'Signing On...' : 'Sign On (ENTER)'}
            </Button>
            
            <Button
              type="button"
              variant="secondary"
              onClick={handleExit}
              disabled={state.isSubmitting}
              className="flex-1"
            >
              Exit (F3)
            </Button>
          </div>
        </form>

        {/* Instructions */}
        <div className="mt-6 text-center text-sm text-gray-500">
          <p>Press ENTER to sign on or F3 to exit</p>
        </div>
      </div>
    </div>
  );
};

export default SignOn;