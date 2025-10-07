'use client';

import React, { useState } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { SignonRequestDTO } from '@/types/user';
import { userService } from '@/services/userService';

interface UserSignonFormProps {
  onSignonSuccess: (userType: 'ADMIN' | 'GENERAL', redirectProgram: string) => void;
  onExit: () => void;
}

export function UserSignonForm({ onSignonSuccess, onExit }: UserSignonFormProps) {
  const [formData, setFormData] = useState<SignonRequestDTO>({
    userId: '',
    password: ''
  });
  const [errors, setErrors] = useState<{ [key: string]: string }>({});
  const [message, setMessage] = useState('');
  const [loading, setLoading] = useState(false);

  // COSGN00C business rule: Handle form submission (ENTER key)
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setErrors({});
    setMessage('');

    try {
      const response = await userService.signon(formData);
      
      if (response.success) {
        onSignonSuccess(response.userType, response.redirectProgram);
      } else {
        setMessage(response.message);
      }
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Authentication failed';
      setMessage(errorMessage);
    } finally {
      setLoading(false);
    }
  };

  // COSGN00C business rule: Handle PF3 key (Exit)
  const handleExit = async () => {
    try {
      const response = await userService.exit();
      setMessage(response.message);
      setTimeout(() => {
        onExit();
      }, 2000);
    } catch (error) {
      console.error('Exit error:', error);
      onExit();
    }
  };

  // COSGN00C business rule: Handle invalid key press
  const handleInvalidKey = async () => {
    try {
      const response = await userService.handleInvalidKey();
      setMessage(response.message);
    } catch (error) {
      setMessage('Invalid key pressed');
    }
  };

  const handleInputChange = (field: keyof SignonRequestDTO) => (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    const value = e.target.value;
    
    // COBOL field length constraints
    const maxLength = field === 'userId' ? 8 : 8; // Both fields are 8 characters max
    const truncatedValue = value.substring(0, maxLength);
    
    setFormData(prev => ({
      ...prev,
      [field]: field === 'userId' ? truncatedValue.toUpperCase() : truncatedValue
    }));
    
    // Clear field-specific errors
    if (errors[field]) {
      setErrors(prev => ({ ...prev, [field]: '' }));
    }
  };

  // Clear form (PF4 equivalent)
  const handleClear = () => {
    setFormData({ userId: '', password: '' });
    setErrors({});
    setMessage('');
  };

  return (
    <div className="min-h-screen bg-gray-100 flex items-center justify-center">
      <div className="bg-white p-8 rounded-lg shadow-md w-full max-w-md">
        {/* Screen Header - COSGN00C layout */}
        <div className="mb-6 text-center">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">CARDDEMO</h1>
          <h2 className="text-lg text-gray-600 mb-4">Sign On</h2>
          <div className="text-sm text-gray-500 space-y-1">
            <div>Program: COSGN00C | Transaction: CC00</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })} {new Date().toLocaleTimeString('en-US', { 
              hour12: false 
            })}</div>
          </div>
        </div>

        <form onSubmit={handleSubmit} className="space-y-4">
          <Input
            label="User ID"
            type="text"
            value={formData.userId}
            onChange={handleInputChange('userId')}
            maxLength={8}
            placeholder="Enter User ID (max 8 chars)"
            error={errors.userId}
            autoFocus
            required
          />

          <Input
            label="Password"
            type="password"
            value={formData.password}
            onChange={handleInputChange('password')}
            maxLength={8}
            placeholder="Enter Password (max 8 chars)"
            error={errors.password}
            required
          />

          {/* Message area */}
          {message && (
            <div className={`p-3 rounded text-sm ${
              message.includes('successful') || message.includes('Thank you') 
                ? 'bg-green-100 text-green-700' 
                : 'bg-red-100 text-red-700'
            }`}>
              {message}
            </div>
          )}

          {/* Function Keys - COSGN00C business rules */}
          <div className="space-y-2">
            <Button
              type="submit"
              className="w-full"
              disabled={loading}
            >
              {loading ? 'Signing In...' : 'Sign In (ENTER)'}
            </Button>

            <div className="grid grid-cols-3 gap-2">
              <Button
                type="button"
                variant="secondary"
                onClick={handleExit}
                disabled={loading}
              >
                Exit (PF3)
              </Button>

              <Button
                type="button"
                variant="outline"
                onClick={handleClear}
                disabled={loading}
              >
                Clear (PF4)
              </Button>

              <Button
                type="button"
                variant="destructive"
                onClick={handleInvalidKey}
                disabled={loading}
              >
                Invalid Key
              </Button>
            </div>
          </div>
        </form>

        {/* Instructions */}
        <div className="mt-6 text-xs text-gray-500 text-center">
          <p>Enter your User ID and Password, then press Sign In</p>
          <p>Use Exit (PF3) to quit the application</p>
        </div>
      </div>
    </div>
  );
}