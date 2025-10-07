'use client';

import React, { useState, useEffect, useRef, KeyboardEvent, FormEvent } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Modal } from '@/components/ui/Modal';
import { SignonRequestDTO, SignonResponseDTO, ValidationError } from '@/types/userSecurity';

interface SignonFormProps {
  onSignonSuccess?: (response: SignonResponseDTO) => void;
  onSignonFailure?: (error: string) => void;
  onExit?: () => void;
  className?: string;
}

interface FormData {
  userId: string;
  password: string;
}

interface FormErrors {
  userId?: string;
  password?: string;
  general?: string;
}

const SignonForm: React.FC<SignonFormProps> = ({
  onSignonSuccess,
  onSignonFailure,
  onExit,
  className = ''
}) => {
  const [formData, setFormData] = useState<FormData>({
    userId: '',
    password: ''
  });

  const [errors, setErrors] = useState<FormErrors>({});
  const [isLoading, setIsLoading] = useState(false);
  const [showModal, setShowModal] = useState(false);
  const [modalMessage, setModalMessage] = useState('');
  const [modalType, setModalType] = useState<'success' | 'error' | 'info'>('info');

  const userIdRef = useRef<HTMLInputElement>(null);
  const passwordRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    // Focus on User ID field when component mounts
    if (userIdRef.current) {
      userIdRef.current.focus();
    }
  }, []);

  const validateForm = (): boolean => {
    const newErrors: FormErrors = {};

    // Validate User ID (SEC-USR-ID: max 8 chars)
    if (!formData.userId.trim()) {
      newErrors.userId = 'USER ID REQUIRED';
    } else if (formData.userId.trim().length > 8) {
      newErrors.userId = 'USER ID MUST NOT EXCEED 8 CHARACTERS';
    } else if (!/^[A-Za-z0-9]+$/.test(formData.userId.trim())) {
      newErrors.userId = 'USER ID MUST CONTAIN ONLY LETTERS AND NUMBERS';
    }

    // Validate Password
    if (!formData.password) {
      newErrors.password = 'PASSWORD REQUIRED';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleInputChange = (field: keyof FormData, value: string) => {
    // Clear errors when user starts typing
    if (errors[field]) {
      setErrors(prev => ({ ...prev, [field]: undefined }));
    }

    // Handle User ID case conversion and length limit
    if (field === 'userId') {
      value = value.toUpperCase().slice(0, 8);
    }

    setFormData(prev => ({
      ...prev,
      [field]: value
    }));
  };

  const handleKeyPress = (event: KeyboardEvent<HTMLInputElement | HTMLFormElement>) => {
    const key = event.key;

    // Handle ENTER key for authentication
    if (key === 'Enter') {
      event.preventDefault();
      handleSubmit();
      return;
    }

    // Handle F3 key for exit (simulated with Escape key)
    if (key === 'Escape' || key === 'F3') {
      event.preventDefault();
      handleExit();
      return;
    }

    // Handle Tab key for field navigation
    if (key === 'Tab') {
      return; // Allow default tab behavior
    }

    // For input fields, handle invalid key presses
    if (event.target instanceof HTMLInputElement) {
      const inputName = event.target.name;
      
      if (inputName === 'userId') {
        // User ID field: only allow alphanumeric characters
        if (!/^[A-Za-z0-9]$/.test(key) && !['Backspace', 'Delete', 'ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(key)) {
          event.preventDefault();
          showMessage('INVALID KEY - USER ID ACCEPTS ONLY LETTERS AND NUMBERS', 'error');
        }
      }
    }
  };

  const handleSubmit = async (event?: FormEvent<HTMLFormElement>) => {
    if (event) {
      event.preventDefault();
    }

    if (isLoading) return;

    // Validate form
    if (!validateForm()) {
      const firstError = Object.values(errors)[0];
      if (firstError) {
        showMessage(firstError, 'error');
      }
      return;
    }

    setIsLoading(true);
    setErrors({});

    try {
      const signonRequest: SignonRequestDTO = {
        userId: formData.userId.trim().toUpperCase(),
        password: formData.password
      };

      const response = await fetch('/api/auth/signon', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(signonRequest),
      });

      const result: SignonResponseDTO = await response.json();

      if (result.success && result.user) {
        showMessage(`WELCOME ${result.user.firstName} ${result.user.lastName}`, 'success');
        
        // Store authentication token if provided
        if (result.token) {
          localStorage.setItem('auth_token', result.token);
        }

        // Call success callback after a brief delay to show the message
        setTimeout(() => {
          onSignonSuccess?.(result);
        }, 1500);
      } else {
        const errorMessage = result.message || 'INVALID USER ID OR PASSWORD';
        setErrors({ general: errorMessage });
        showMessage(errorMessage, 'error');
        onSignonFailure?.(errorMessage);
        
        // Clear password field and focus on User ID
        setFormData(prev => ({ ...prev, password: '' }));
        setTimeout(() => {
          if (userIdRef.current) {
            userIdRef.current.focus();
            userIdRef.current.select();
          }
        }, 100);
      }
    } catch (error) {
      const errorMessage = 'SYSTEM ERROR - PLEASE TRY AGAIN';
      setErrors({ general: errorMessage });
      showMessage(errorMessage, 'error');
      onSignonFailure?.(errorMessage);
      
      // Clear password field and focus on User ID
      setFormData(prev => ({ ...prev, password: '' }));
      setTimeout(() => {
        if (userIdRef.current) {
          userIdRef.current.focus();
        }
      }, 100);
    } finally {
      setIsLoading(false);
    }
  };

  const handleExit = () => {
    if (onExit) {
      onExit();
    } else {
      // Default exit behavior - could redirect to home page
      window.location.href = '/';
    }
  };

  const showMessage = (message: string, type: 'success' | 'error' | 'info') => {
    setModalMessage(message);
    setModalType(type);
    setShowModal(true);
  };

  const closeModal = () => {
    setShowModal(false);
    setModalMessage('');
  };

  const handleUserIdKeyDown = (event: KeyboardEvent<HTMLInputElement>) => {
    handleKeyPress(event);
    
    // Move to password field on Tab or Enter if User ID is valid
    if ((event.key === 'Tab' || event.key === 'Enter') && formData.userId.trim() && !errors.userId) {
      if (event.key === 'Enter') {
        event.preventDefault();
        passwordRef.current?.focus();
      }
    }
  };

  const handlePasswordKeyDown = (event: KeyboardEvent<HTMLInputElement>) => {
    handleKeyPress(event);
  };

  return (
    <>
      <div className={`max-w-md mx-auto mt-8 p-6 bg-white border border-gray-300 shadow-lg ${className}`}>
        <div className="text-center mb-6">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">SYSTEM SIGN-ON</h1>
          <div className="text-sm text-gray-600">
            <p>ENTER - Sign On</p>
            <p>F3/ESC - Exit</p>
          </div>
        </div>

        <form onSubmit={handleSubmit} onKeyDown={handleKeyPress} className="space-y-4">
          <div>
            <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-1">
              USER ID:
            </label>
            <Input
              ref={userIdRef}
              id="userId"
              name="userId"
              type="text"
              value={formData.userId}
              onChange={(e) => handleInputChange('userId', e.target.value)}
              onKeyDown={handleUserIdKeyDown}
              className={`font-mono uppercase ${errors.userId ? 'border-red-500' : ''}`}
              placeholder="Enter User ID"
              maxLength={8}
              autoComplete="username"
              disabled={isLoading}
            />
            {errors.userId && (
              <p className="text-red-600 text-sm mt-1 font-medium">{errors.userId}</p>
            )}
          </div>

          <div>
            <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-1">
              PASSWORD:
            </label>
            <Input
              ref={passwordRef}
              id="password"
              name="password"
              type="password"
              value={formData.password}
              onChange={(e) => handleInputChange('password', e.target.value)}
              onKeyDown={handlePasswordKeyDown}
              className={`font-mono ${errors.password ? 'border-red-500' : ''}`}
              placeholder="Enter Password"
              autoComplete="current-password"
              disabled={isLoading}
            />
            {errors.password && (
              <p className="text-red-600 text-sm mt-1 font-medium">{errors.password}</p>
            )}
          </div>

          {errors.general && (
            <div className="bg-red-50 border border-red-200 rounded-md p-3">
              <p className="text-red-700 text-sm font-medium">{errors.general}</p>
            </div>
          )}

          <div className="flex space-x-3 pt-4">
            <Button
              type="submit"
              variant="primary"
              size="md"
              disabled={isLoading}
              className="flex-1"
            >
              {isLoading ? 'SIGNING ON...' : 'SIGN ON (ENTER)'}
            </Button>
            
            <Button
              type="button"
              variant="outline"
              size="md"
              onClick={handleExit}
              disabled={isLoading}
              className="flex-1"
            >
              EXIT (F3)
            </Button>
          </div>
        </form>

        <div className="mt-6 text-center text-xs text-gray-500">
          <p>User ID: Maximum 8 characters, letters and numbers only</p>
          <p>Use ENTER to sign on, F3 or ESC to exit</p>
        </div>
      </div>

      <Modal
        isOpen={showModal}
        onClose={closeModal}
        title={modalType === 'success' ? 'SUCCESS' : modalType === 'error' ? 'ERROR' : 'INFORMATION'}
        className="max-w-sm"
      >
        <div className="text-center py-4">
          <div className={`text-lg font-medium ${
            modalType === 'success' ? 'text-green-700' : 
            modalType === 'error' ? 'text-red-700' : 
            'text-blue-700'
          }`}>
            {modalMessage}
          </div>
          <div className="mt-4">
            <Button
              onClick={closeModal}
              variant={modalType === 'success' ? 'primary' : 'outline'}
              size="sm"
            >
              OK
            </Button>
          </div>
        </div>
      </Modal>
    </>
  );
};

export default SignonForm;