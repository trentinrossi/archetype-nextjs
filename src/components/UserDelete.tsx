'use client';

import { useState, useEffect, useCallback, useRef } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { userService } from '@/services/userService';
import { 
  UserResponse,
  UserFormErrors, 
  UserType,
  USER_TYPE_LABELS,
  USER_VALIDATION_RULES 
} from '@/types/user';

interface UserDeleteProps {
  initialUserId?: string;
  onDelete?: (userId: string) => Promise<void>;
  onCancel?: () => void;
  className?: string;
}

interface DeleteFormData {
  userId: string;
  firstName: string;
  lastName: string;
  userType: UserType;
}

export default function UserDelete({ 
  initialUserId = '',
  onDelete, 
  onCancel, 
  className = '' 
}: UserDeleteProps) {
  const [formData, setFormData] = useState<DeleteFormData>({
    userId: initialUserId,
    firstName: '',
    lastName: '',
    userType: UserType.USER,
  });

  const [errors, setErrors] = useState<UserFormErrors>({});
  const [loading, setLoading] = useState(false);
  const [lookupLoading, setLookupLoading] = useState(false);
  const [message, setMessage] = useState<{ type: 'success' | 'error' | 'info'; text: string } | null>(null);
  const [userFound, setUserFound] = useState(false);

  // Refs for keyboard navigation
  const userIdRef = useRef<HTMLInputElement>(null);
  const deleteButtonRef = useRef<HTMLButtonElement>(null);
  const clearButtonRef = useRef<HTMLButtonElement>(null);
  const cancelButtonRef = useRef<HTMLButtonElement>(null);

  const fieldRefs = [
    userIdRef,
    deleteButtonRef,
    clearButtonRef,
    cancelButtonRef,
  ];

  const validateUserId = useCallback((userId: string): string | undefined => {
    if (!userId.trim()) {
      return 'User ID is required';
    }
    if (userId.length > USER_VALIDATION_RULES.userId.maxLength) {
      return `User ID must be ${USER_VALIDATION_RULES.userId.maxLength} characters or less`;
    }
    return undefined;
  }, []);

  const handleInputChange = useCallback((value: string) => {
    setFormData(prev => ({ ...prev, userId: value }));
    
    // Clear field error when user starts typing
    if (errors.userId) {
      setErrors(prev => ({ ...prev, userId: undefined }));
    }
    
    // Clear general error
    if (errors.general) {
      setErrors(prev => ({ ...prev, general: undefined }));
    }
    
    // Clear message
    if (message) {
      setMessage(null);
    }

    // Reset user found state when changing user ID
    if (userFound) {
      setUserFound(false);
      setFormData(prev => ({
        ...prev,
        firstName: '',
        lastName: '',
        userType: UserType.USER,
      }));
    }
  }, [errors, message, userFound]);

  const handleUserLookup = useCallback(async () => {
    const userIdError = validateUserId(formData.userId);
    if (userIdError) {
      setErrors({ userId: userIdError });
      setMessage({ type: 'error', text: 'User ID can NOT be empty...' });
      return;
    }

    try {
      setLookupLoading(true);
      setMessage(null);
      setErrors({});

      const user: UserResponse = await userService.getUserById(formData.userId);
      
      setFormData(prev => ({
        ...prev,
        userId: user.userId,
        firstName: user.firstName,
        lastName: user.lastName,
        userType: user.userType,
      }));

      setUserFound(true);
      setMessage({ type: 'info', text: 'Press PF5 key to delete this user ...' });

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'User lookup failed';
      setMessage({ 
        type: 'error', 
        text: errorMessage.includes('not found') ? 'User ID NOT found...' : 'Unable to lookup User...' 
      });
      setErrors({ userId: errorMessage });
      setUserFound(false);
      setFormData(prev => ({
        ...prev,
        firstName: '',
        lastName: '',
        userType: UserType.USER,
      }));
    } finally {
      setLookupLoading(false);
    }
  }, [formData.userId, validateUserId]);

  const handleClear = useCallback(() => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      userType: UserType.USER,
    });
    setErrors({});
    setMessage(null);
    setUserFound(false);
    
    // Focus on User ID field
    setTimeout(() => {
      userIdRef.current?.focus();
    }, 0);
  }, []);

  const handleDelete = useCallback(async () => {
    const userIdError = validateUserId(formData.userId);
    if (userIdError) {
      setErrors({ userId: userIdError });
      setMessage({ type: 'error', text: 'User ID can NOT be empty...' });
      return;
    }

    if (!userFound) {
      setMessage({ type: 'error', text: 'Please lookup a user first' });
      return;
    }

    try {
      setLoading(true);
      setMessage(null);
      setErrors({});

      if (onDelete) {
        await onDelete(formData.userId);
      } else {
        // Default behavior - call userService directly
        await userService.deleteUser(formData.userId);
      }

      setMessage({ type: 'success', text: `User ${formData.userId} has been deleted ...` });
      
      // Clear form after successful deletion
      setFormData({
        userId: '',
        firstName: '',
        lastName: '',
        userType: UserType.USER,
      });
      setUserFound(false);
      
      // Focus back on User ID field
      setTimeout(() => {
        userIdRef.current?.focus();
      }, 100);

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to delete user';
      setMessage({ 
        type: 'error', 
        text: errorMessage.includes('not found') ? 'User ID NOT found...' : 'Unable to Update User...' 
      });
      setErrors({ general: errorMessage });
    } finally {
      setLoading(false);
    }
  }, [formData.userId, userFound, validateUserId, onDelete]);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        if (!userFound && formData.userId.trim()) {
          event.preventDefault();
          handleUserLookup();
        }
        break;
      case 'F3':
        if (onCancel) {
          event.preventDefault();
          onCancel();
        }
        break;
      case 'F4':
        event.preventDefault();
        handleClear();
        break;
      case 'F5':
        event.preventDefault();
        if (userFound) {
          handleDelete();
        }
        break;
      case 'F12':
        if (onCancel) {
          event.preventDefault();
          onCancel();
        }
        break;
      case 'Escape':
        if (onCancel) {
          event.preventDefault();
          onCancel();
        }
        break;
      case 'Tab':
        // Allow default tab behavior
        break;
      case 'ArrowDown':
        event.preventDefault();
        const activeDownElement = document.activeElement;
        const currentDownIndex = fieldRefs.findIndex(ref => ref.current === activeDownElement);
        
        if (currentDownIndex >= 0 && currentDownIndex < fieldRefs.length - 1) {
          const nextRef = fieldRefs[currentDownIndex + 1];
          nextRef.current?.focus();
        }
        break;
      case 'ArrowUp':
        event.preventDefault();
        const activeUpElement = document.activeElement;
        const currentUpIndex = fieldRefs.findIndex(ref => ref.current === activeUpElement);
        
        if (currentUpIndex > 0) {
          const prevRef = fieldRefs[currentUpIndex - 1];
          prevRef.current?.focus();
        }
        break;
    }
  }, [formData.userId, userFound, handleUserLookup, handleDelete, handleClear, onCancel, fieldRefs]);

  useEffect(() => {
    const handleGlobalKeyDown = (event: KeyboardEvent) => {
      handleKeyDown(event);
    };

    window.addEventListener('keydown', handleGlobalKeyDown);
    return () => {
      window.removeEventListener('keydown', handleGlobalKeyDown);
    };
  }, [handleKeyDown]);

  useEffect(() => {
    // Focus on User ID field when component mounts
    setTimeout(() => {
      userIdRef.current?.focus();
    }, 100);
  }, []);

  useEffect(() => {
    // Auto-lookup if initialUserId is provided
    if (initialUserId && !userFound) {
      handleUserLookup();
    }
  }, [initialUserId, userFound, handleUserLookup]);

  return (
    <div className={`max-w-2xl mx-auto p-6 bg-white border rounded-lg shadow-sm ${className}`}>
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Delete User (COUSR03C)</h1>
        <p className="text-sm text-gray-600 mt-1">
          Lookup and delete user from the system
        </p>
      </div>

      {/* Success/Error Messages */}
      {message && (
        <div className={`mb-6 p-4 rounded-md ${
          message.type === 'success' 
            ? 'bg-green-50 border border-green-200 text-green-800' 
            : message.type === 'info'
            ? 'bg-blue-50 border border-blue-200 text-blue-800'
            : 'bg-red-50 border border-red-200 text-red-800'
        }`}>
          <p className="font-medium">{message.text}</p>
        </div>
      )}

      {/* General Error */}
      {errors.general && (
        <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-md">
          <p className="text-red-800 font-medium">Error: {errors.general}</p>
        </div>
      )}

      {/* Form */}
      <form onSubmit={(e) => e.preventDefault()} className="space-y-6">
        {/* User ID Lookup */}
        <div>
          <div className="flex gap-2">
            <Input
              ref={userIdRef}
              label="User ID *"
              value={formData.userId}
              onChange={(e) => handleInputChange(e.target.value.toUpperCase())}
              error={errors.userId}
              placeholder="Enter User ID to lookup"
              maxLength={8}
              disabled={loading || lookupLoading}
              className="font-mono flex-1"
            />
            {!userFound && (
              <Button
                onClick={handleUserLookup}
                disabled={loading || lookupLoading || !formData.userId.trim()}
                className="mt-6"
              >
                {lookupLoading ? 'Looking up...' : 'Lookup (Enter)'}
              </Button>
            )}
          </div>
          <p className="text-xs text-gray-500 mt-1">
            Enter User ID and press Enter or click Lookup
          </p>
        </div>

        {/* User Details (only shown after successful lookup) */}
        {userFound && (
          <>
            {/* User Information Display */}
            <div className="bg-gray-50 p-4 rounded-md border">
              <h3 className="text-lg font-medium text-gray-900 mb-4">User Information</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    First Name
                  </label>
                  <div className="p-2 bg-white border rounded-md text-gray-900">
                    {formData.firstName}
                  </div>
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    Last Name
                  </label>
                  <div className="p-2 bg-white border rounded-md text-gray-900">
                    {formData.lastName}
                  </div>
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    User Type
                  </label>
                  <div className="p-2 bg-white border rounded-md text-gray-900">
                    {USER_TYPE_LABELS[formData.userType]} ({formData.userType})
                  </div>
                </div>
              </div>
            </div>

            {/* Warning Message */}
            <div className="bg-yellow-50 border border-yellow-200 rounded-md p-4">
              <div className="flex">
                <div className="flex-shrink-0">
                  <svg className="h-5 w-5 text-yellow-400" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                  </svg>
                </div>
                <div className="ml-3">
                  <h3 className="text-sm font-medium text-yellow-800">
                    Warning: This action cannot be undone
                  </h3>
                  <div className="mt-2 text-sm text-yellow-700">
                    <p>
                      You are about to permanently delete user <strong>{formData.userId}</strong> ({formData.firstName} {formData.lastName}).
                      This action will remove all user data and cannot be reversed.
                    </p>
                  </div>
                </div>
              </div>
            </div>

            {/* Action Buttons */}
            <div className="flex flex-wrap gap-3 pt-6 border-t">
              <Button
                ref={deleteButtonRef}
                onClick={handleDelete}
                disabled={loading}
                variant="destructive"
                className="min-w-[140px]"
              >
                {loading ? 'Deleting...' : 'Delete User (F5)'}
              </Button>

              <Button
                ref={clearButtonRef}
                onClick={handleClear}
                disabled={loading}
                variant="outline"
                className="min-w-[120px]"
              >
                Clear (F4)
              </Button>

              {onCancel && (
                <Button
                  ref={cancelButtonRef}
                  onClick={onCancel}
                  disabled={loading}
                  variant="outline"
                >
                  Cancel (F3/F12)
                </Button>
              )}
            </div>
          </>
        )}
      </form>

      {/* Navigation Instructions */}
      <div className="mt-8 p-4 bg-gray-50 rounded-md">
        <h3 className="text-sm font-medium text-gray-900 mb-2">Keyboard Navigation:</h3>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs text-gray-600">
          <div>Enter: Lookup User</div>
          <div>F3: Exit/Cancel</div>
          <div>F4: Clear Form</div>
          <div>F5: Delete User</div>
          <div>F12: Cancel</div>
          <div>↑/↓: Navigate Fields</div>
        </div>
      </div>

      {/* Form Status */}
      <div className="mt-4 text-xs text-gray-500 text-center">
        {userFound && <span className="text-red-600">● User ready for deletion</span>}
        {!userFound && <span className="text-gray-400">● Enter User ID to begin</span>}
      </div>
    </div>
  );
}